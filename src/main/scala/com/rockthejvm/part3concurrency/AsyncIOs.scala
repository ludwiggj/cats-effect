package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*
import scala.util.Try

object AsyncIOs extends IOApp.Simple {
  val threadPool = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  private def computeMeaningOfLife(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing the meaning of life on some other thread")
    42
  }

  def computeMeaningOfLifeEither(): Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit =
    threadPool.execute(new Runnable {
      override def run(): Unit = computeMeaningOfLifeEither()
    })

  def computeMolOnThreadPoolSingleAbstractMethod(): Unit =
    threadPool.execute(() => computeMeaningOfLifeEither()) // The argument "implements" Runnable (SAM)

  // lift computation to an IO
  // computation is run on a thread pool that is not controlled by CE
  // async is a FFI (Foreign Function Interface)

  // CE thread evaluating this IO blocks (semantically) until this cb is invoked (by some other thread)
  val asyncMolIO: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaningOfLifeEither()
      cb(result) // CE thread is notified with the result
    }
  }

  // Exercise - generalise - lift an async computation on ec to an IO
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
    IO.async_ { (cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try {computation()}.toEither
        cb(result)
      }
    }
  }

  val asyncMol_v2 = asyncToIO(computeMeaningOfLife)(ec)

  // Exercise - lift an async computation as a Future, to an IO
  lazy val molFuture:Future[Int] = Future { computeMeaningOfLife() }(ec)

  def futureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete { tryResult => cb(tryResult.toEither) }(ec)
    }

  val asyncMol_v3 = futureToIO(molFuture)

  // IO has explicit method to create an IO from a future
  val asyncMol_v4 = IO.fromFuture(IO(molFuture)) // future suspended in IO to ensure it's not evaluated eagerly

  // Exercise - a never-ending IO - no callback no finish!
  def neverEndingIO[A](computation: () => A)(ec: ExecutionContext): IO[A] = {
    IO.async_ { (_: Callback[A]) =>
      ec.execute { () =>
        computation()
        ()
      }
    }
  }

  def neverEndingIO_v2[A](): IO[A] = {
    IO.async_ { _ => ()}
  }

  val neverEnding = neverEndingIO(computeMeaningOfLife)(ec)
  val neverEnding_v2 = neverEndingIO_v2()

  // IO supports never - but it is implemented using the full fat async method
  IO.never

  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
      // needs a finaliser in case the computation is cancelled
      // finalisers are of type IO[Unit]
      // but don't have to specify a finaliser => Option[IO[Unit]]
      // creating an option is an effect(!!!)  => IO[Option[IO[Unit]]]

      // return an IO[Option[IO[Unit]]]

      IO {
        threadPool.execute { () =>
          val result = computeMeaningOfLifeEither()
          cb(result)
        }
      } // IO[Unit]
        .as(Some(IO("Cancelled!").debug.void)) // finalizer - executed if the effect is cancelled
    }

    for {
      fib <- asyncMeaningOfLifeIO_v2.start
      _ <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  override def run: IO[Unit] =
    demoAsyncCancellation().debug >> IO(threadPool.shutdown())
    // neverEnding_v2.debug >> IO(threadPool.shutdown())
    // neverEnding.debug >> IO(threadPool.shutdown())
    // asyncMol_v4.debug >> IO(threadPool.shutdown())
    // asyncMol_v3.debug >> IO(threadPool.shutdown())
    // asyncMol_v2.debug >> IO(threadPool.shutdown())
    // asyncMolIO.debug >> IO(threadPool.shutdown())
}
