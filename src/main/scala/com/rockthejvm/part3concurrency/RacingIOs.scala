package com.rockthejvm.part3concurrency

import cats.effect.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object RacingIOs extends IOApp.Simple {
  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"Starting computation of $value").debug >>
        IO.sleep(duration) >>
        IO(s"Computation of $value: done") >>
        IO(value)
      ).onCancel(IO(s"Computation CANCELLED for $value").debug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    // both IOs run on separate fibers
    // the first one to finish will complete the result
    // the loser will be cancelled

    first.flatMap {
      case Left(mol) => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"Fav lang won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang = runWithSleep("Scala", 2.seconds)

    // Race pair provides much more control - loser is not automatically cancelled
    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (loser fiber, winner result)
    ]] = IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  // Exercises
  // 1 - implement a timeout pattern with race
  // 2 - a method to return a LOSING effect from a race (hint: use racePair)
  // 3 - implement race in terms of racePair

  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    IO.race(IO.sleep(duration), io).flatMap {
      case Left(()) => IO.raiseError(new RuntimeException(s"Calculation timed out after $duration"))
      case Right(result) => IO(result)
    }

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibBLoser)) => fibBLoser.join.flatMap {
        case Succeeded(fb) => fb.map(Right.apply)
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException(s"Loser B has been cancelled"))
      }
      case Right((fibALoser, _)) => fibALoser.join.flatMap {
        case Succeeded(fa) => fa.map(Left.apply)
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException(s"Loser A has been cancelled"))
      }
    }
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> IO("A won").debug >> effectA.map(Left.apply)
        case Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => IO("B won").debug >> effectB.map(Right.apply)
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }

      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> IO("B won").debug >> effectB.map(Right.apply)
        case Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => IO("A won").debug >> effectA.map(Left.apply)
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }
    }

  override def run: IO[Unit] =
//     simpleRace(IO.sleep(1.second) >> IO("hello"), IO.sleep(500.millis) >> IO("winner")).debug.void
     simpleRace(IO.sleep(500.millis) >> IO("winner"), IO.sleep(1.second) >> IO("dolly")).debug.void
    // unrace(IO.sleep(1.second) >> IO("hello"), IO.sleep(500.millis) >> IO("winner")).debug.void
    // unrace(IO.sleep(500.millis) >> IO("winner"), IO.sleep(1.second) >> IO("dolly")).debug.void

    // Timeout is also available as a method on IO
    // (IO.sleep(1.second) >> IO("hello")).timeout(2.seconds).debug.void
    // (IO.sleep(1.second) >> IO("hello")).timeout(500.millis).debug.void // throws a TimeoutException

    // timeout(IO.sleep(1.second) >> IO("hello").debug, 2.seconds).void
    // timeout(IO.sleep(1.second) >> IO("hello").debug, 500.milli).void
    // testRacePair().void
    // testRace().debug.void
}
