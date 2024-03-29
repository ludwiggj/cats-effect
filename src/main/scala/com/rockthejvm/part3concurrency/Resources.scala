package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import com.rockthejvm.part3concurrency.Resources.Connection
import com.rockthejvm.utils.*

import java.io.{File, FileReader}
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

object Resources extends IOApp.Simple {

  // use case - manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").debug

    def close: IO[String] = IO(s"closing connection to $url").debug
  }

  // this leaks resources i.e. close is not called
  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open >> IO.sleep(Int.MaxValue.seconds)).start // alternative is IO.never
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  val correctAsyncFetchUrl = for {
    connection <- IO(new Connection("rockthejvm.com"))
    fib <- (connection.open >> IO.sleep(Int.MaxValue.seconds)).onCancel(connection.close.void).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // bracket pattern: someIO.bracket(useResourceCb)(releaseResourceCb) - releaseResourceCb is called no matter what
  // bracket is equivalent to try-catches (pure FP)
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open >> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  /**
   * Exercise: read a file with the bracket pattern
   * - open a scanner
   * - read the file line by line, every 100 millis
   * - close the scanner
   * - if canceled / throws error, close the scanner
   */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] = {
    if (scanner.hasNextLine) {
      IO(scanner.nextLine()).debug >> IO.sleep(200.millis) >> readLineByLine(scanner)
    } else {
      IO.unit
    }
  }

  def bracketFileScanner(path: String) = IO(s"Opening file at path: $path").debug >>
    openFileScanner(path)
      .bracket(
        scanner => readLineByLine(scanner)
      )(
        scanner => IO(s"Closing file at path: $path").debug >> IO(scanner.close())
      )

  // Don't need to start a new fiber - bracket takes care of this for us
  val fileScannerProgram = for {
    fib <- bracketFileScanner("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").start
    _ <- fib.join
  } yield ()

  // Resources
  // Want to open two resources
  // Nesting resources using bracket is tedious - equivalent of try catch finally
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      // acquire a connection based on the contents of the file (config)
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open >> IO.never
      }(conn => conn.close.void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))

  // Bracket has 3 components:
  //  - resource acquisition
  //  - resource usage
  //  - resource release

  // Resource is a data structure. It has 2 components that describe:
  //  - effect of acquiring a resource
  //  - effect of releasing a resource
  //  i.e. the resource can be used later - the releasing takes care of itself

  // Separation of concerns makes the code a lot more readable
  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close.void)
  // usage is at a later point in the code

  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn => conn.open >> IO.never).start // we can do whatever we like here - we know that
    // the resource will be released properly at the end
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource).void
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(r => usingResource(r).void)

  // Exercise - read a text file with one line every 100 millis, using Resource
  //            i.e. refactor the bracket exercise to use resource
  def fileResource(path: String) = {
    def acquireFile(path: String): IO[Scanner] = IO(s"Opening file at path: $path").debug >> openFileScanner(path)
    def closeFile(path: String, scanner: Scanner) = IO(s"Closing file at path: $path").debug >> IO(scanner.close())

    Resource.make(acquireFile(path))(scanner => closeFile(path, scanner))
  }

  def readFileLineByLine(path: String) = fileResource(path).use(readLineByLine)

  // resource can handle cancellation
  def cancelReadFile(path: String) = for {
    fib <- readFileLineByLine(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  def connectionResource(url: String) = {
    val acquireConnection = IO(new Connection(url))
    val closeConnection: Connection => IO[Unit] = conn => conn.close.void
    Resource.make(acquireConnection)(closeConnection)
  }

  // combine nested resources via flatmap - connection + file resources will be closed automatically in correct sequence
  def connFromConfigResource(path: String): Resource[IO, Connection] = for {
    fileReader <- fileResource(path)
    connection <- connectionResource(fileReader.nextLine())
  } yield connection

  def openConnection(path: String): IO[Unit] = connFromConfigResource(path).use { conn =>
    conn.open >> IO.never
  }

  // resource can handle cancellation of nested resources
  def cancelOpenConnection(path: String) = for {
    fib <- openConnection(path).start
    _ <- IO.sleep(2.seconds) >> fib.cancel
  } yield ()

  // can attach finaliser to regular IOs
  val ioWithFinaliser = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinaliser2 = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"freeing resource: $result").debug).void
    case Errored(_) => IO("nothing to release").debug.void
    case Canceled() => IO("resource was cancelled, releasing what's left").debug.void
  }

  override def run: IO[Unit] =
    ioWithFinaliser.void
  // cancelOpenConnection("src/main/resources/connection.txt")
  // cancelOpenConnection("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  // openConnection("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  // cancelReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  // readFileLineByLine("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
  // usingResourceWithResource
  // usingResourceWithBracket
  // resourceFetchUrl
  // bracketFileScanner("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
}
