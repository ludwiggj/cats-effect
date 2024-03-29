package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*
import scala.concurrent.duration.*

object CancellingIOs extends IOApp.Simple {
  /* Cancelling IOs
     - fib.cancel
     - IO.race & other APIs
     - manual cancellation
   */

  // Manual cancellation - everything after IO.canceled is not run
  val chainOfIOs: IO[Int] = (IO("waiting").debug >> IO.canceled >> IO(42).debug).debug

  // uncancellable
  // example: online store, payment processor
  // payment process must NOT be cancelled
  val specialPaymentSystem = (
    IO("payment running, don't cancel").debug >>
      IO.sleep(1.second) >>
      IO("payment completed").debug
    ).onCancel(IO("MEGA CANCEL OF DOOM").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable // same thing

  // Cannot be cancelled
  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  // The uncancelable API is more complex and more general
  // It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
  // The Poll object can be used to mark sections within the returned effect which CAN BE CANCELLED

  // Example: authentication service. Has two parts:
  // - input passwrod, can be cancelled, because otherwise we might block indefinitely on user input e.g. user makes a cup of tea
  // - verify password, CANNOT be cancelled once it's started

  val inputPassword = IO("Input password").debug >> IO("typing password").debug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")
  val verifyPassword = (pw: String) => IO("Verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- inputPassword.onCancel(IO("Authentication timed out. Try again later.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful").debug
      else IO("Authentication failed").debug
    } yield ()
  }

  // Uncancellable calls are masks which suppress cancellation
  // Poll calls are "gaps opened" in the uncancellable region
  // In this case I've renamed poll to canCancel
  val authFlow_v2: IO[Unit] = IO.uncancelable { canCancel =>
    for {
      pw <- canCancel(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void) // this can be cancelled
      verified <- verifyPassword(pw) // this cannot be cancelled
      _ <- if (verified) IO("Authentication successful").debug // this cannot be cancelled
      else IO("Authentication failed").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  val authProgram_v2_cancelInputPassword = for {
    authFib <- authFlow_v2.start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  val authProgram_v2_cancelVerifyPassword = for {
    authFib <- authFlow_v2.start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // Exercises
  // 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug

  // uncancelable will eliminate ALL cancel points (apart from those marked with poll)
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)

  // 2
  // wrapping the flow in another uncancelable will mean that it can't be cancelled
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow_v2).start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // IO.uncancelable(poll => poll(authFlow_v2)) leaves the flow unchanged - they cancel each other out
  val authProgram_v2_cancelInputPassword_no_change = for {
    authFib <- IO.uncancelable(poll => poll(authFlow_v2)).start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  val authProgram_v2_cancelVerifyPassword_no_change = for {
    authFib <- IO.uncancelable(poll => poll(authFlow_v2)).start
    _ <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // 3
  // cancel signal is processed by first cancellable region after the cancel was sent
  def threeStepProgram(delay: FiniteDuration): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancellable").debug >> IO.sleep(1.second) >> IO("cancellable end").debug) >>
      IO("uncancellable").debug >> IO.sleep(1.second) >> IO("uncancellable end").debug >>
      poll(IO("second cancellable").debug >> IO.sleep(1.second) >> IO("second cancellable end").debug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(delay) >> IO("CANCELLING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] =
    threeStepProgram(1500.millis)
  // authProgram_v2_cancelVerifyPassword_no_change
  // authProgram_v2_cancelInputPassword_no_change
  // invincibleAuthProgram
  // uncancelableMol.void
  // cancelBeforeMol.void
  // authProgram_v2_cancelVerifyPassword
  // authProgram_v2_cancelInputPassword
  // authProgram
  // authFlow
  // noCancellationOfDoom
  // cancellationOfDoom
  // chainOfIOs.void
}
