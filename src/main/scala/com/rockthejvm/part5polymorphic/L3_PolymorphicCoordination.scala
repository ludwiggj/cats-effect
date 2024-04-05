package com.rockthejvm.part5polymorphic

import cats.effect.{Concurrent, Deferred, Fiber, IO, IOApp, Outcome, OutcomeIO, Ref, Spawn}
import com.rockthejvm.utils.general.*

import scala.concurrent.duration.*

object L3_PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for any effect type

  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]

    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO] // given instance of Concurrent[IO]
  val aDeferred = Deferred[IO, Int] // requires a given Concurrent[IO] to be in scope

  // can create a deferred via concurrentIO
  val aDeferred_v2 = concurrentIO.deferred[Int]

  // can create a ref via concurrentIO
  val aRef = concurrentIO.ref(42)

  // capabilities build on top of other typeclasses i.e.
  // pure, map/flatMap, raiseError, uncancelable, start (fibers), +ref, +deferred

  // Generalise the alarm solution

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  private def alarmNotification[F[_]](using concurrent: Concurrent[F]): F[Unit] = {
    def alarm(signal: Deferred[F, Int]): F[Unit] = for {
      _ <- concurrent.pure("[alarm] waiting for signal...").debug
      count <- signal.get
      _ <- concurrent.pure(s"[alarm] BEEP! BEEP! $count").debug
    } yield ()

    def counter(count: Ref[F, Int], signal: Deferred[F, Int]): F[Unit] = for {
      _ <- concurrent.unsafeSleep(1.second)
      newCount <- count.updateAndGet(_ + 1)
      _ <- concurrent.pure(s"[counter] $newCount...").debug
      _ <- if (newCount == 10) signal.complete(newCount).void else counter(count, signal)
    } yield ()

    for {
      count <- concurrent.ref(0)
      signal <- concurrent.deferred[Int]
      alarmFib <- concurrent.start(alarm(signal))
      counterFib <- concurrent.start(counter(count, signal))
      _ <- alarmFib.join
      _ <- counterFib.join
    } yield ()
  }

  // Exercises
  // 1. Generalise racePair
  // 2. Generalise the Mutex concurrency primitive for any F

  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B]) // (loser fiber, winner result)
  ]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  /*
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
   */

  import cats.effect.syntax.spawn.*
  import cats.effect.syntax.monadCancel.*
  
  def ourRacePairCancellable[F[_], A, B](fa: F[A], fb: F[B])(using concurrent: Concurrent[F]): F[RaceResult[F, A, B]] =
    concurrent.uncancelable { poll =>
      for {
        _ <- concurrent.pure("About to start the race").debug
        signal <- concurrent.deferred[EitherOutcome[F, A, B]]
        fibA <- fa.guaranteeCase {
          case outcomeA => signal.complete(Left(outcomeA)) >> concurrent.pure("fibA produced an outcome").debug.void
        }.start
        fibB <- fb.guaranteeCase {
          case outcomeB => signal.complete(Right(outcomeB)) >> concurrent.pure("fibB produced an outcome").debug.void
        }.start
        // this thread is blocking on the signal - should be cancellable, if both fibers take forever
        outcome <- concurrent.pure("Blocking on signal for race result").debug >> poll(signal.get).onCancel {
          for {
            cancelFibA <- concurrent.pure("Cancelling fibA").debug >> fibA.cancel.start
            cancelFibB <- concurrent.pure("Cancelling fibB").debug >> fibB.cancel.start
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

  override def run: IO[Unit] = cancelOurRace()
}

// Cats & Cats Effect TC type hierarchy:
/*
    ____________          _____________          __________       ______________
   |           |         |            |         |         |      |             |
   | Semigroup |         |  Foldable  |         | Functor |      | Semigroupal |
   |  combine  |         |            |         |   map   |      |   product   |
   |___________|         |____________|         |_________|      |_____________|
        ^                       ^                 ^     ^               ^
        |                       |_________________|     |_______________|
        |                                |                      |
    ____________                   _____________          ______________
   |           |                  |            |         |             |
   |  Monoid   |                  |  Traverse  |         |    Apply    |
   |  empty    |                  |  traverse  |         |     ap      |
   |___________|                  |____________|         |_____________|
                                                               ^
                                                               |
                                                    _________________________
                                                   |                        |
                                            ______________           ______________
                                           |             |          |             |
                                           |   FlatMap   |          | Applicative |
                                           |   flatMap   |          |    pure     |
                                           |_____________|          |_____________|
                                                  ^                      ^  ^
                                                  |______________________|  |__________
                                                               |                      |
                                                           __________         ___________________
                                                          |         |        |                  |
                                                          |  Monad  |        | ApplicativeError |
                                                          |         |        |    raiseError    |
                                                          |         |        |  handleErrorWith |
                                                          |_________|        |__________________|
                                                               ^                     ^
                                                               |_____________________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |  MonadError  |
                                                                  |    ensure    |
                                                                  |______________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |  MonadCancel |
                                                                  |   canceled   |
                                                                  | uncancelable |
                                                                  |______________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |     Spawn    |
                                                                  |     start    |
                                                                  |______________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |  Concurrent  |
                                                                  |     ref      |
                                                                  |   deferred   |
                                                                  |______________|

*/