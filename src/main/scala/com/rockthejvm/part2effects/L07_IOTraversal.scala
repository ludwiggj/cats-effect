package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}
import cats.Traverse // Traverse
import com.rockthejvm.utils.debug
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Random

object L07_IOTraversal extends IOApp.Simple {
  def heavyComputation(string: String): Int = {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  def heavyComputationFuture(string: String): Future[Int] = Future {
    heavyComputation(string)
  }

  val workload: List[String] = List("I quite like CE", "Scala is fab", "Yo mama likes awesome stuff")

  def clunkyFutures(): IO[Unit] = {
    println("List of futures...")
    val futures: List[Future[Int]] = workload.map(heavyComputationFuture)
    futures.foreach(_.foreach(println))
    Thread.sleep(1000) // ensure the future completes by waiting for a second
    IO.unit
  }

  // Much easier to wait on single future rather than multiple futures

  def traverseFutures(): IO[Unit] = {
    val singleFuture: Future[List[Int]] = Traverse[List].traverse(workload)(heavyComputationFuture)

    // Better to await on future
    println("Future of lists...")
    IO.println(Await.result(singleFuture, Duration.Inf))
  }

  def computeAsIO(string: String): IO[Int] = IO {
    heavyComputation(string)
  }.debug

  def traverseIOs(): IO[Unit] = {
    println("Traverse IOs...")
    Traverse[List].traverse(workload)(computeAsIO).map(_.sum).debug.void
  }

  def parallelTraverseIOs(): IO[Unit] = {
    println("Parallel traverse IOs...")
    import cats.syntax.parallel.*
    workload.parTraverse(computeAsIO).map(_.sum).debug.void
  }

  // Exercises
  import cats.syntax.traverse.*

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.traverse(identity)

  def sequenceGeneral[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  import cats.syntax.parallel.*

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  def parSequenceGeneral[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(identity)

  def sequences(): IO[Unit] = {
    val futures: List[Future[Int]] = workload.map(heavyComputationFuture)
    val inverted: Future[List[Int]] = Traverse[List].sequence(futures)

    val ios: List[IO[Int]] = workload.map(computeAsIO)
    val inverted1: IO[List[Int]] = parSequence(ios).debug
    val inverted2: IO[List[Int]] = ios.parSequence.debug // extension from Parallel syntax import

    for {
      _ <- IO.println("Sequenced future of list...")
      _ <- IO(Await.result(inverted, Duration.Inf)).debug
      _ <- IO.println("Parallel sequenced IO of list...")
      _ <- inverted1
      _ <- IO.println("Another parallel sequenced IO of list...")
      _ <- inverted2
    } yield ()
  }

  override def run: IO[Unit] =
    clunkyFutures() >>
    traverseFutures() >>
    traverseIOs() >>
    parallelTraverseIOs() >>
    sequences()
}
