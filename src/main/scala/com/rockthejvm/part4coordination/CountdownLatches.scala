package com.rockthejvm.part4coordination

import cats.effect.{Deferred, IO, IOApp, Resource}
import cats.effect.std.CountDownLatch
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}
import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Random

object MyCountDownLatch {
  type Signal = Deferred[IO, Unit]

  sealed trait State

  case object Done extends State

  case class Live(count: Int, signal: Signal) extends State

  def create(count: Int): IO[CountDownLatch[IO]] = for {
    signal <- Deferred[IO, Unit]
    state <- IO.ref[State](Live(count, signal))
  } yield new CountDownLatch:
    // if latch is currently unlocked, state is unchanged
    // if latch is locked, add new signal to the queue and wait on it
    override def await: IO[Unit] =
      // this is a smell - modify doesn't actually modify the state!
      //        state.modify {
      //          case Done =>
      //            (Done, IO("Await: Latch is open").debug >> IO.unit)
      //          case Live(count, signal) => (
      //            Live(count, signal),
      //            IO(s"Await: Latch is closed, count: $count - blocking").debug >> signal.get
      //          )
      //        }.flatten

      // don't care if fiber waiting on the mutex is cancelled
      state.get.flatMap {
        case Done =>
          IO("Await: Latch is open").debug >> IO.unit
        case Live(_, signal) => (
          IO(s"Await: Latch is closed, count: $count - blocking").debug >> signal.get
          )
      }

    // if latch is already unlocked, leave state unchanged (shouldn't happen)
    // otherwise decrement latch count
    //   - if count hits 0, release waiting threads
    //   - otherwise carry on
    override def release: IO[Unit] =
      state.modify {
        case Done => (
          Done,
          IO("Release: Latch is already open").debug >> IO.unit
        )
        case Live(1, signal) =>
          (
            Done,
            IO(s"Release: Latch releasing: 0 - time to GO!").debug >> signal.complete(()).void
          )
        case Live(count, signal) =>
          (
            Live(count - 1, signal),
            IO(s"Release: Latch count: ${count - 1}").debug >> IO.unit
          )
      }.flatten.uncancelable // so that all the waiting fibers aren't blocked
  // TODO - when talking about mutex.release, said that state.modify is atomic! So what's different here?
}

object CountdownLatches extends IOApp.Simple {

  // Countdown latches are a coordination primitive initialised with a count
  // All fibers calling await() on the latch are (semantically) blocked
  // When the internal count of the latch reaches 0 (via release() calls
  // from other fibers), all waiting fibers are unblocked

  // Block a number of fibers until another thread gives the go signal
  private def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("GO GO GO!").debug
  } yield ()

  private def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug
    _ <- latch.await
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  def sprint(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    _ <- (1 to 10).toList.parTraverse { id =>
      createRunner(id, latch)
    }
    _ <- announcerFib.join
  } yield ()

  private def sprint2(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    // this gets stuck
    _ <- (1 to 10).toList.parTraverse { id =>
      createRunner(id, latch)
    }
    _ <- announcer(latch)
  } yield ()

  // Exercise - simulate a file downloader on multiple threads
  private object FileServer {
    private val fileChunksList = List(
      "I love scala",
      "Cats Effect seems quite fun",
      "Never would I have thought that I would do low-level concurrency with pure FP"
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)

    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  private def writeToFile(path: String, contents: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use { writer =>
      IO(writer.write(contents))
    }
  }

  private def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) => IO(reader.getLines().foreach(writer.write))
    }
  }

  // - call file server API and get the number of chunks (n)
  // - start a countdown latch
  // - start n fibers which download a chunk of the file (use the file server's download chunk API)
  // - block on the latch until each task has finished
  // - after all the chunks are done, stitch the files together under the same file on disk

  private def filePartFilePath(filename: String, destFolder: String, chunkId: Int): String =
    s"$destFolder/$filename.part$chunkId"

  private def createFileDownloaderTask(filename: String, destFolder: String, chunkId: Int, latch: CountDownLatch[IO]): IO[Unit] =
    for {
      _ <- IO(s"[chunk: $chunkId] Getting chunk").debug
      _ <- IO.sleep((Random.nextDouble * 1000).toInt.millis * 3)
      contents <- FileServer.getFileChunk(chunkId)
      _ <- IO(s"[chunk: $chunkId] Writing content '$contents'").debug
      _ <- writeToFile(filePartFilePath(filename, destFolder, chunkId), contents)
      _ <- IO(s"[chunk: $chunkId] Chunk download complete").debug
      _ <- latch.release
    } yield ()

  def downloadFile(filename: String, destFolder: String): IO[Unit] = {
    for {
      chunkCount <- FileServer.getNumChunks
      latch <- CountDownLatch[IO](chunkCount)
      // TODO need to start parTraverse on a separate fiber, otherwise the latch functionality isn't demonstrated!
      _ <- (0 until chunkCount).toList.parTraverse { chunkId =>
        createFileDownloaderTask(filename, destFolder, chunkId, latch)
      }.start
      _ <- IO("about to block on latch").debug
      _ <- latch.await
      _ <- (0 until chunkCount).toList.traverse {
        chunkId => appendFileContents(filePartFilePath(filename, destFolder, chunkId), s"$destFolder/$filename")
      }
    } yield ()
  }

  private def myDownloadFile(filename: String, destFolder: String): IO[Unit] = {
    for {
      chunkCount <- FileServer.getNumChunks
      latch <- MyCountDownLatch.create(chunkCount)
      _ <- (0 until chunkCount).toList.parTraverse { chunkId =>
        createFileDownloaderTask(filename, destFolder, chunkId, latch)
      }.start
      _ <- latch.await
      _ <- (0 until chunkCount).toList.traverse {
        chunkId => appendFileContents(filePartFilePath(filename, destFolder, chunkId), s"$destFolder/$filename")
      }
    } yield ()
  }

  // exercise - implement your own countdown latch with ref and deferred
  override def run: IO[Unit] =
    myDownloadFile("result.txt", "src/main/resources/countdown")
  //     downloadFile("result.txt", "src/main/resources/countdown")
}
