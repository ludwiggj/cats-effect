package com.rockthejvm.part5polymorphic

import cats.effect.{Concurrent, IO, IOApp, Temporal}

import scala.concurrent.duration.*
import com.rockthejvm.utils.general.*

object L4_PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal - ability to create time-limited blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit] // semantically blocks this fiber for a specified time
    // also unlocks timeout functionality
  }
  // abilities: pure, map/flatMap, raiseError, uncancelable, start (fibers), ref, deferred, +sleep
  val temporalIO = Temporal[IO] // given Temporal[IO] in scope

  val chainOfEffects = IO("Loading...").debug >> IO.sleep(1.second) >> IO("Game ready!").debug.void
  val chainOfEffects_v2 = temporalIO.pure("Loading...").debug >> temporalIO.sleep(1.second) >> temporalIO.pure("Game ready!").debug.void

  // Exercise - timeout
  import cats.syntax.flatMap.*
  import cats.effect.syntax.spawn.*
  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(using temporal: Temporal[F]): F[A] =
    temporal.sleep(duration).race(fa).flatMap {
      case Left(()) => temporal.raiseError(new RuntimeException(s"Calculation timed out after $duration"))
      case Right(result) => temporal.pure(result)
    }
    
  // timeout is actually implemented in GenTemporal

  override def run =
    // timeout(IO.sleep(1.second) >> IO("hello"), 2.seconds).debug.void
    timeout(IO.sleep(1.second) >> IO("hello"), 500.millis).debug.void
    // chainOfEffects_v2
    // chainOfEffects
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
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |   Temporal   |
                                                                  |     sleep    |
                                                                  |______________|


*/