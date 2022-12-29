package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.* // debug extension method
import cats.syntax.apply.* // mapN method
import cats.Parallel

object L06_IOParallelism extends IOApp.Simple {
  // IOs are usually sequential
  val firstIO = IO(s"[${Thread.currentThread().getName}] First")
  val secondIO = IO(s"[${Thread.currentThread().getName}] Second")

  val composedIO = for {
    first <- firstIO
    second <- secondIO
  } yield s"$first and $second love Rock the JVM"

  val meaningOfLife: IO[Int] = IO(42)
  val favLang: IO[String] = IO("scala")

  // Also sequential
  val lifeGoals = (meaningOfLife.debug, favLang.debug).mapN((num, string) => s"My life goals: $num, $string").debug

  // parallel IOs
  // convert a sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug) // parallel version
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)    // parallel version

  import cats.effect.implicits.* // Needed for next statement
  val lifeGoalsParallel: IO.Par[String] = (parIO1, parIO2).mapN((num, string) => s"My life goals: $num, $string")
  // Covert parallel to sequential so we can evaluate it
  val lifeGoalsParallel2: IO[String] = Parallel[IO].sequential(lifeGoalsParallel)

  // Shorthand for all of the above
  import cats.syntax.parallel.* // for parMapN
  val lifeGoalsParallelShorthand: IO[String] =
    (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"My shorthand life goals: $num, $string")

  // Regarding failure
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this"))

  // Compose success and failure
  // Fails...
  //   [io-compute-2] 42
  //   Exception in thread "main" java.lang.RuntimeException: I can't do this
  val parallelWithFailure: IO[String] = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  // Compose two failures
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))

  // First IO fails first - Exception in thread "main" java.lang.RuntimeException: I can't do this
  val twoFailures: IO[String] = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  // Second IO fails first - Exception in thread "main" java.lang.RuntimeException: Second failure
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(2000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = for {
    _ <- IO.println("Compose two IOs sequentially via for:")
    _ <- composedIO.map(println)
    _ <- IO.println("Compose two IOs sequentially via mapN:")
    _ <- lifeGoals.map(println)
    _ <- IO.println("Compose two IOs in parallel via mapN:")
    _ <- lifeGoalsParallel2.debug.void
    _ <- IO.println("Compose two IOs in parallel via parMapN:")
    _ <- lifeGoalsParallelShorthand.debug.void
    _ <- IO.println("Compose success and failure:")
    // _ <- parallelWithFailure.void // no point debugging output as exception is thrown
    _ <- parallelWithFailure.handleError(ex => s"Oh dear: $ex").debug.void
    _ <- IO.println("Compose two failures, first fails first:")
    _ <- twoFailures.handleError(ex => s"Oh dear: $ex").debug.void
    _ <- IO.println("Compose two failures, second fails first:")
    // Waits until both have completed
    _ <- twoFailuresDelayed.handleError(ex => s"Oh dear: $ex").debug.void
  } yield ()
}
