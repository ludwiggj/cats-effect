package com.rockthejvm.part2effects

import cats.effect.{IO, IOApp}
import cats.Traverse // Traverse
import com.rockthejvm.utils.debug
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Random

object L07_IOTraversal extends IOApp.Simple {
  def compute(string: String): Int = {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  def computeAsFuture(string: String): Future[Int] = Future {
    compute(string)
  }

  val workload: List[String] = List("I quite like CE", "Scala is fab", "Yo mama likes awesome stuff")
  val listTraverse = Traverse[List]

  def clunkyFutures(): IO[Unit] = {
    val futures: List[Future[Int]] = workload.map(computeAsFuture)
    futures.foreach(_.foreach(println))
    Thread.sleep(1000)
    IO.unit
  }

  // Much easier to wait on single future rather than multiple futures
  def traverseFutures(): IO[Unit] = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(computeAsFuture)
    // IO.println(Await.result(singleFuture, Duration.Inf))
    IO {
      singleFuture.foreach(println)
      Thread.sleep(1000)
    }
  }

  def computeAsIO(string: String): IO[Int] = IO {
    compute(string)
  }.debug

  def traverseIOs(): IO[Unit] = {
    listTraverse.traverse(workload)(computeAsIO).map(_.sum).debug.void
  }

  def parallelTraverseIOs(): IO[Unit] = {
    import cats.syntax.parallel.*
    workload.parTraverse(computeAsIO).map(_.sum).debug.void
  }

  // Exercises
  import cats.syntax.traverse.*

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.traverse(identity)

  def sequenceGeneral[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(listOfIOs)(identity)

  import cats.syntax.parallel.*

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  def parSequenceGeneral[F[_]: Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] =
    listOfIOs.parTraverse(identity)

  def sequences(): IO[Unit] = {
    val futures: List[Future[Int]] = workload.map(computeAsFuture)
    val inverted: Future[List[Int]] = listTraverse.sequence(futures)
    val ios: List[IO[Int]] = workload.map(computeAsIO)

    IO(Await.result(inverted, Duration.Inf)).debug >>
      parSequence(ios).debug >>
      ios.parSequence.debug.void // from Parallel syntax import
  }

  override def run: IO[Unit] =
    clunkyFutures() >>
    traverseFutures() >>
    traverseIOs() >>
    parallelTraverseIOs() >>
    sequences()
}
