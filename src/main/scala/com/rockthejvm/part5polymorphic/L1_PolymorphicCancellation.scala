package com.rockthejvm.part5polymorphic

import cats.effect.kernel.MonadCancel
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.{Applicative, Functor, Monad}
import cats.effect.{GenSpawn, IO, IOApp, Poll}

import scala.concurrent.duration.*

object L1_PolymorphicCancellation extends IOApp.Simple {
  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]

    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F] {
    // ensure - not relevant here
  }

  // Think of this as a higher kinded function type i.e. not A => A but F[A] => F[A]
  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  // MonadCancel
  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    // This is the new functionality that MyMonadCancel adds in
    def canceled: F[Unit]

    def uncancelable[A](poll: Poll[F] => F[A]): F[A] // Poll function is curried i.e. F[A] => F[A] => F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]

  // We can create values (because of all the functionality that MonadCancel inherits)
  val molIO: IO[Int] = monadCancelIO.pure(42)
  // It's a monad so we can map
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ * 10)

  // When using IO.uncancelable in previous exercises, it's using the underlying MonadCancel instance for IO
  val mustCompute: IO[Int] = monadCancelIO.uncancelable { _ => // Poll, not used in this example
    for {
      _ <- monadCancelIO.pure("once started, I can't go back...")
      res <- monadCancelIO.pure(56)
    } yield res
  }

  // MonadCancel exists because we can generalise these computations for any effect
  // for which there is a MonadCancel in scope. For example:

  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map

  // can generalise code - make it applicable for any effect type for which you have an implicit or given typeclass
  // instance of that (effect) type in scope
  def mustComputeGeneral[F[_], E](using mc: MonadCancel[F, E]): F[Int] = mc.uncancelable { _ => // Poll, not used in this example
    for {
      _ <- mc.pure("once started, I can't go back...")
      res <- mc.pure(56)
    } yield res
  }

  val mustCompute_v2 = mustComputeGeneral[IO, Throwable] // this works 'for free'

  // what other functionality does MonadCancel offer?

  // allow cancellation listeners (onCancel, which uses the MonadCancel under the hood)
  val mustComputeWithListener = mustCompute.onCancel(IO("I'm being cancelled").void) // calling onCancel on an IO
  val mustComputeWithListener_v2 = monadCancelIO.onCancel(mustCompute, IO("I'm being cancelled").void) // calling onCancel on the MonadCancel instance

  // .onCancel as extension method
  // import cats.effect.syntax.monadCancel.* // .onCancel

  // allow finalisers (important when talking about Ref and Deferred in polymorphic terms)
  // Supports guarantee and guaranteeCase
  val aComputationWithFinalisers = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(a => IO(s"successful: $a").void)
    case Errored(e) => IO(s"failed: $e").void
    case Canceled() => IO("canceled").void
  }

  // bracket pattern is specific to MonadCancel
  // i.e. the acquisition of effects and the releasing of resources happens because there is a MonadCancel in scope
  // Resource acquisition (the Resource type) also needs a MonadCancel instance in scope
  val aComputationWithUsage = monadCancelIO.bracket(IO(42)) { value =>
    IO(s"Using the meaning of life: $value")
  } { value =>
    IO(s"Releasing the meaning of file: $value").void
  }

  /*
   Exercise - generalise a piece of code
   */

  // Code to generalise

  /*
  import com.rockthejvm.utils.*

  val inputPassword = IO("Input password").debug >> IO("typing password").debug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")
  val verifyPassword = (pw: String) => IO("Verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // this can be cancelled
      verified <- verifyPassword(pw) // this cannot be cancelled
      _ <- if (verified) IO("Authentication successful").debug // this cannot be cancelled
      else IO("Authentication failed").debug
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  */

  // Not using this one...
  // def unsafeSleep[F[_], E](duration: FiniteDuration)(using mc: MonadCancel[F, E]): F[Unit] =
  //  mc.pure(Thread.sleep(duration.toMillis))

  import com.rockthejvm.utils.general.*
  import cats.effect.syntax.monadCancel.* // .onCancel extension method
  import cats.effect.syntax.spawn.* // .start extension method

  def inputPassword[F[_], E](using mc: MonadCancel[F, E]): F[String] = for {
    _ <- mc.pure("Input password").debug
    _ <- mc.pure("typing password").debug
    _ <- mc.unsafeSleep(5.seconds)
    pw <- mc.pure("RockTheJVM1!")
  } yield pw

  def verifyPassword[F[_], E](pw: String)(using mc: MonadCancel[F, E]): F[Boolean] = for {
    _ <- mc.pure("Verifying...").debug
    _ <- mc.unsafeSleep(5.seconds)
    isValid <- mc.pure(pw == "RockTheJVM1!")
  } yield isValid

  def authFlow[F[_], E](using gs: GenSpawn[F, E]): F[Unit] = {
    val flow = gs.uncancelable { poll =>
      for {
        pw <- poll(inputPassword).onCancel(gs.pure("Authentication timed out. Try again later.").debug.void) // this can be cancelled
        verified <- verifyPassword(pw) // this cannot be cancelled
        _ <- if (verified)
          gs.pure("Authentication successful").debug // this cannot be cancelled
        else
          gs.pure("Authentication failed").debug
      } yield ()
    }

    for {
      authFib <- flow.start
      _ <- gs.unsafeSleep(3.seconds)
      _ <- gs.pure("Authentication timed out, attempting cancel...").debug
      _ <- authFib.cancel
      _ <- authFib.join
    } yield ()
  }

  def authFlow2[F[_], E](using mc: MonadCancel[F, E]): F[Unit] = {
    mc.uncancelable { poll =>
      for {
        pw <- poll(inputPassword).onCancel(mc.pure("Authentication timed out. Try again later.").debug.void) // this can be cancelled
        verified <- verifyPassword(pw) // this cannot be cancelled
        _ <- if (verified)
          mc.pure("Authentication successful").debug // this cannot be cancelled
        else
          mc.pure("Authentication failed").debug
      } yield ()
    }
  }

  val program = for {
    authFib <- authFlow2[IO, Throwable].start
    _ <- IO.sleep(1.seconds)
    _ <- IO.pure("Authentication timed out, attempting cancel...").debug
    _ <- authFib.cancel
    _ <- authFib.join
  } yield ()

  override def run: IO[Unit] =
    program
    // authFlow[IO, Throwable]
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

*/