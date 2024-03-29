package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import com.rockthejvm.utils.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("scala")

  def sameThreadIOs() = for {
    _ <- meaningOfLife.debug
    _ <- favLang.debug
  } yield ()

  // introduce the fiber
  // description of a computation that will be run on some thread managed by CE runtime
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  val mol: IO[Int] = meaningOfLife.debug

  // start a computation i.e. allocate a fiber to run it
  // the fiber is not started - the fiber allocation is wrapped in an effect (suspended in an IO)
  // as it's an effectful operation
  val aFiber: IO[Fiber[IO, Throwable, Int]] = mol.start

  def differentThreadIOs() = for {
    _ <- aFiber // Run on a different thread - not sure if they are still sequenced or not - see next method for answer
    _ <- favLang.debug
  } yield ()

  def differentThreadWillItWaitIOs() = for {
    _ <- {
      IO.sleep(1.second) >>
        IO.pure(42)
    }.debug.start
    _ <- favLang.debug
  } yield ()

  // joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fibre to terminate (like an IO[A], but type signature is more complicated)
  } yield result
  // IO[ResultType of fib.join]
  // fib.join = Outcome[IO, Throwable, A]

  // Possible outcomes:
  //   - success with an IO
  //   - failure with an exception
  //   - cancelled

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(0)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel() = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    // onCancel is a finalizer, allowing you to free up resources
    val taskWithCancellationHandler = task.onCancel(
      IO("I'm being deplatformed!").debug.void
    )

    for {
      fib <- taskWithCancellationHandler.start // on a separate thread
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug // running on the calling thread
      _ <- fib.cancel // on calling thread
      result <- fib.join // on calling thread
    } yield result
  }

  /**
   * Exercises
   * 1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
   *    - return the result in an IO
   *    - if errored or cancelled, return a failed IO
   */

  // Exercise 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val fiberResult = for {
      fib <- io.debug.start
//      fib <- io.start
//      _ <- fib.cancel
      result <- fib.join
    } yield result

    fiberResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e) => IO.raiseError(new RuntimeException(s"Error result: ${e.toString}"))
      case Canceled() => IO.raiseError(new RuntimeException("Computation was cancelled"))
    }
  }

  /**
   * 2. Write a function that takes two IOs, runs them on different fibres and returns an IO with a tuple containing both results.
   *    - if both IOs complete successfully, tuple their results
   *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
   *    - if the first IO doesn't error but second IO returns an error, raise that error
   *    - if one (or both) cancelled, raise a RuntimeException
   */

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val fiberResults = for {
      fibA <- ioa.start
//      _ <- fibA.cancel
      resultA <- fibA.join
      fibB <- iob.start
//      _ <- fibB.cancel
      resultB <- fibB.join
    } yield (resultA, resultB)

    fiberResults.flatMap {
      case (Succeeded(fa), Succeeded(fb)) => fa.flatMap(a => fb.map(b => (a, b)))
      case (Errored(ea), _) => IO.raiseError(new RuntimeException(s"First computation error result: ${ea.toString}"))
      case (_, Errored(eb)) => IO.raiseError(new RuntimeException(s"Second computation error result: ${eb.toString}"))
      case (Canceled(), Canceled()) => IO.raiseError(new RuntimeException("Both computations were cancelled"))
      case (Canceled(), _) => IO.raiseError(new RuntimeException("First computation was cancelled"))
      case (_, Canceled()) => IO.raiseError(new RuntimeException("Second computation was cancelled"))
    }
  }

  /**
   * 3. Write a function that adds a timeout to an IO:
   *    - IO runs on a fiber
   *    - if the timeout duration passes, then the fiber is cancelled
   *    - the method returns an IO[A] which contains
   *      - the original value if the computation is successful before the timeout signal
   *      - the exception if the computation is failed before the timeout signal
   *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
   */

    // 3
    def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
      val fiberResult = for {
        fib <- io.start
        _ <- (IO("timeout: sleeping").debug >> IO.sleep(duration) >> IO("timeout: cancelling").debug >> fib.cancel).start // a potential leak
        result <- fib.join
      } yield result

      fiberResult.flatMap {
        case Succeeded(fa) => fa
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Calculation timed out - cancelled"))
      }
    }

  override def run: IO[Unit] = {
    //    sameThreadIOs()
    //    differentThreadIOs()
    //    differentThreadWillItWaitIOs() // It won't wait
    //    differentThreadWillItWaitIOs() >> IO.sleep(2.seconds) // Second result appears as we force it to wait

    //    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
    //      .debug.void                       // Succeeded(IO(42))

    //    throwOnAnotherThread().debug.void
    //    testCancel().debug.void

    //    processResultsFromFiber(IO.pure("15")).debug.flatMap(IO.println)
    processResultsFromFiber(IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug >> IO.pure(42)).flatMap(IO.println)
    //    processResultsFromFiber(IO.raiseError[String](new RuntimeException("oh dear"))).attempt.flatMap(IO.println)

    // To test cancellation
    //    processResultsFromFiber(IO.pure("15").debug >> IO.sleep(1.second)).attempt.flatMap(IO.println)

    //    tupleIOs(IO.pure("15"), IO.pure(15)).debug.flatMap(IO.println)
    //    tupleIOs(IO.raiseError(new RuntimeException("rats")), IO.pure(15)).attempt.flatMap(IO.println)
    //    tupleIOs(IO.pure(15), IO.raiseError(new RuntimeException("double rats"))).attempt.flatMap(IO.println)

    //    tupleIOs(IO.sleep(2.seconds) >> IO(1).debug, IO.sleep(4.seconds) >> IO(2).debug).debug.flatMap(IO.println)

    // To test cancellation
    //    tupleIOs(IO.pure("15") >> IO.sleep(1.second), IO.pure(15) >> IO.sleep(1.second)).attempt.flatMap(IO.println)

    // timeout(IO.pure("15").debug, 500.millis).flatMap(IO.println)
    // timeout(IO.sleep(1.second) >> IO.pure("15").debug, 500.millis).flatMap(IO.println)
    // timeout(IO.raiseError[Int](new RuntimeException("triple rats")), 500.millis).flatMap(IO.println)
  }
}
