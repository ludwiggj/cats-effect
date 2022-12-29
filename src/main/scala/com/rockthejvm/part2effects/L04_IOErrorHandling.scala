package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.either.*

import scala.util.{Failure, Success, Try}

object L04_IOErrorHandling {
  // create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new Exception("A failure"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("A proper failure"))

  // handle exceptions
  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("Still here!"))
  }

  val dealWithIt2 = aFailure.handleErrorWith {
    _ => IO.delay(println("Still here!"))
  }

  // turn it into an either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and success in one go
  val resultAsString = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

  // redeemWith
  val resultAsEffect = aFailure.redeemWith(ex => IO.println(s"FAIL: $ex"), value => IO.println(s"SUCCESS: $value"))

  // Exercises
  // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match
    case None => IO.raiseError(ifEmpty)
    case Some(value) => IO(value) // can do IO.pure(value) to force eager evaluation

  def option2IOAlternative[A](option: Option[A])(ifEmpty: Throwable): IO[A] = IO.fromOption(option)(ifEmpty)

  def try2IO[A](aTry: Try[A]): IO[A] =
    aTry match
      case Failure(exception) =>
        IO.raiseError(exception)
      case Success(value) =>
        IO(value)

  def try2IOAlternative[A](aTry: Try[A]): IO[A] = IO.fromTry(aTry)

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
    anEither match
      case Left(exception) => IO.raiseError(exception)
      case Right(value) => IO(value)

  def either2IOAlternative[A](anEither: Either[Throwable, A]): IO[A] = IO.fromEither(anEither)

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.handleError(handler)

  def handleIOErrorAlternative[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.handleErrorWith(handler)

  def handleIOErrorWithAlternative[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    // aFailedCompute.unsafeRunSync()
    // aFailure.unsafeRunSync()
    dealWithIt.unsafeRunSync()
    dealWithIt2.unsafeRunSync()
    println(effectAsEither.unsafeRunSync())
    println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()

    println(option2IO(Some(2))(new RuntimeException("A proper failure")).unsafeRunSync())
    // println(option2IO(Option.empty)(new RuntimeException("A proper failure")).unsafeRunSync())

    println(option2IOAlternative(Some(2))(new RuntimeException("A proper failure")).unsafeRunSync())
    // println(option2IOAlternative(Option.empty)(new RuntimeException("A proper failure")).unsafeRunSync())

    println(try2IO(Try(22)).unsafeRunSync())
    // println(try2IO(Try(1 / 0)).unsafeRunSync())

    println(try2IOAlternative(Try(22)).unsafeRunSync())
    // println(try2IOAlternative(Try(1 / 0)).unsafeRunSync())

    println(either2IO(222.asRight).unsafeRunSync())
    // println(either2IO(new RuntimeException("A proper failure").asLeft).unsafeRunSync())

    println(either2IOAlternative(222.asRight).unsafeRunSync())
    //println(either2IOAlternative(new RuntimeException("A proper failure").asLeft).unsafeRunSync())

    println(handleIOError(aFailure)(_ => -1).unsafeRunSync())
    println(handleIOErrorWith(aFailure)(_ => IO(-1)).unsafeRunSync())

    println(handleIOErrorAlternative(aFailure)(_ => -1).unsafeRunSync())
    println(handleIOErrorWithAlternative(aFailure)(_ => IO(-1)).unsafeRunSync())
  }
}
