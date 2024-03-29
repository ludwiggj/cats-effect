package com.rockthejvm.part4coordination

import cats.effect.kernel.Deferred
import cats.effect.{Fiber, FiberIO, IO, IOApp, Outcome, OutcomeIO, Ref}
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple {

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int] // allocating a deferred is also an effect
  val aDeferred_v2: IO[Deferred[IO, Int]] = IO.deferred[Int] // same

  // get blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap {
    deferred => deferred.get // blocks the fiber
  }

  val writer: IO[Boolean] = aDeferred.flatMap { deferred =>
    deferred.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[consumer] waiting for result...").debug
      meaningOfLife <- signal.get
      _ <- IO(s"[consumer] got the result: $meaningOfLife").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[producer] calculating answer....").debug
      _ <- IO.sleep(1.second)
      _ <- IO("[producer] complete: 42").debug
      _ <- signal.complete(42)
    } yield ()

    for {
      signal <- IO.deferred[Int]
      _ <- (consumer(signal), producer(signal)).parTupled
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("I ", "love S", "cala", " with Cat", "s Effect!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO(s"[downloader] got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(_ + part)
      }.sequence.void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("[notifier] file download complete").debug else
        IO("[notifier] downloading").debug >> IO.sleep(500.millis) >> notifyFileComplete(contentRef) // busy wait
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      fibNotifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- fibNotifier.join
    } yield ()
  }

  // deferred works miracles for waiting - no more busy waiting
  def fileNotifierWithDeferred(): IO[Unit] = {
    def downloadFile(deferred: Deferred[IO, String]): IO[Unit] =
      for {
        content <- IO.ref("")
        _ <- fileParts.map { part =>
          for {
            _ <- IO(s"[downloader] got '$part'").debug
            _ <- IO.sleep(1.second)
            downloaded <- content.updateAndGet(_ + part)
            _ <- if (downloaded.endsWith("<EOF>")) deferred.complete(downloaded) else IO.unit
          } yield ()
        }.sequence
      } yield ()

    def notifyFileComplete(deferred: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      result <- deferred.get
      _ <- IO(s"[notifier] downloaded: $result").debug
    } yield ()

    for {
      deferred <- IO.deferred[String]
      fibDownloader <- downloadFile(deferred).start
      fibNotifier <- notifyFileComplete(deferred).start
      _ <- fibDownloader.join
      _ <- fibNotifier.join
    } yield ()
  }

  // Deferred is a purely functional concurrency primitive with two methods
  // get - blocks the fiber (semantically) until a value is present
  // complete - inserts a value that can be read by the blocked fibers

  // why
  // - allows inter-fiber communication
  // - avoids busy waiting
  // - maintains thread safety

  // use-cases
  // - producer-consumer like problems
  // - sending data between fibers
  // - notification mechanisms

  // Exercises
  // (medium) write a small alarm notification with two simultaneous IOs
  //   - one that increments a counter every second (a clock)
  //   - one that waits for the counter to become 10, then prints a message "time's up!"
  private def alarmNotification(): IO[Unit] = {
    def alarm(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("[alarm] waiting for signal...").debug
      count <- signal.get
      _ <- IO(s"[alarm] BEEP! BEEP! $count").debug
    } yield ()

    def counter(count: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      newCount <- count.updateAndGet(_ + 1)
      _ <- IO(s"[counter] $newCount...").debug
      _ <- if (newCount == 10) signal.complete(newCount) else counter(count, signal)
    } yield ()

    for {
      count <- IO.ref(0)
      signal <- IO.deferred[Int]
      _ <- (alarm(signal), counter(count, signal)).parTupled
    } yield ()
  }

  // (mega hard) implement racePair with deferred
  //   - use a deferred which can hold an Either[outcome for ioa, outcome for iob]
  //   - start two fibres, one for each IO
  //   - on completion (with any status), each IO needs to complete that deferred
  //   - (hint: use a finalizer from the Resources lesson)
  //   - (hint2: use a guarantee call to make sure that the fibers complete the Deferred
  //   - what do you do in case of cancellation (the hardest part)?

  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[A, B] = Either[OutcomeIO[A], OutcomeIO[B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] =
    for {
      signal <- IO.deferred[EitherOutcome[A, B]]
      fibA <- ioa.guaranteeCase {
        case outcomeA => signal.complete(Left(outcomeA)) >> IO("fibA produced an outcome").debug.void
      }.start
      fibB <- iob.guaranteeCase {
        case outcomeB => signal.complete(Right(outcomeB)) >> IO("fibB produced an outcome").debug.void
      }.start
      outcome <- signal.get
    } yield {
      outcome match
        case Left(outcomeA) => Left((outcomeA, fibB))
        case Right(outcomeB) => Right((fibA, outcomeB))
    }

  def ourRacePairCancellable[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      _ <- IO("About to start the race").debug
      signal <- IO.deferred[EitherOutcome[A, B]]
      fibA <- ioa.guaranteeCase {
        case outcomeA => signal.complete(Left(outcomeA)) >> IO("fibA produced an outcome").debug.void
      }.start
      fibB <- iob.guaranteeCase {
        case outcomeB => signal.complete(Right(outcomeB)) >> IO("fibB produced an outcome").debug.void
      }.start
      // this thread is blocking on the signal - should be cancellable, if both fibers take forever
      outcome <- IO("Blocking on signal for race result").debug >> poll(signal.get).onCancel {
        for {
          cancelFibA <- IO("Cancelling fibA").debug >> fibA.cancel.start
          cancelFibB <- IO("Cancelling fibB").debug >> fibB.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      }
    } yield {
      outcome match
        case Left(outcomeA) => Left((outcomeA, fibB))
        case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  def cancelOurRace(): IO[Unit] = for {
    raceFib <- ourRacePairCancellable(
      IO.sleep(1.day) >> IO("loser"),
      IO.sleep(500.days) >> IO("loser 2")
    ).start
    _ <- IO("Waiting for 5 seconds...").debug >>
      IO.sleep(5.seconds) >>
      IO("Cancelling...").debug >>
      raceFib.cancel
  } yield ()

  override def run: IO[Unit] =
    cancelOurRace()
    // ourRacePair(IO.sleep(1.second) >> IO("loser"), IO.sleep(500.millis) >> IO("winner")).debug.void
    // ourRacePair(IO.sleep(1.second) >> IO("winner"), IO.sleep(1500.millis) >> IO("loser")).debug.void
    // alarmNotification()
    // fileNotifierWithDeferred()
    // fileNotifierWithRef()
    // demoDeferred()
}
