package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.either.*

import scala.util.{Failure, Success, Try}

object L04_IOErrorHandling {
  def main(args: Array[String]): Unit = {
    // create failed effects
    val aFailedCompute: IO[Int] = IO.delay(throw new Exception("A failure"))
    val aFailure: IO[Int] = IO.raiseError(new RuntimeException("A proper failure"))

    // aFailedCompute.unsafeRunSync()
    // aFailure.unsafeRunSync()

    // handle exceptions
    val dealWithIt = aFailure.handleErrorWith {
      case _: RuntimeException => IO.delay(println("Still here!"))
    }

    dealWithIt.unsafeRunSync()

    val dealWithIt2 = aFailure.handleErrorWith {
      _ => IO.delay(println("Still here!"))
    }

    dealWithIt2.unsafeRunSync()

    // turn it into an either
    val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

    println(effectAsEither.unsafeRunSync())

    // redeem: transform the failure and success in one go
    val resultAsString = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")

    println(resultAsString.unsafeRunSync())

    // redeemWith
    val resultAsEffect = aFailure.redeemWith(ex => IO.println(s"FAIL: $ex"), value => IO.println(s"SUCCESS: $value"))

    resultAsEffect.unsafeRunSync()

    // Exercises
    // 1 - construct potentially failed IOs from standard data types (Option, Try, Either)
    def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match
      case None => IO.raiseError(ifEmpty)
      case Some(value) => IO(value) // can do IO.pure(value) to force eager evaluation

    println(s"option2IO(Some): ${option2IO(Some(2))(new RuntimeException("A proper failure")).unsafeRunSync()}")
    println(s"option2IO(None): ${option2IO(Option.empty)(new RuntimeException("A proper failure")).attempt.unsafeRunSync()}")

    def option2IOAlternative[A](option: Option[A])(ifEmpty: Throwable): IO[A] = IO.fromOption(option)(ifEmpty)

    println(s"option2IOAlternative(Some): ${option2IOAlternative(Some(2))(new RuntimeException("A proper failure")).unsafeRunSync()}")
    println(s"option2IOAlternative(None): ${option2IOAlternative(Option.empty)(new RuntimeException("A proper failure")).attempt.unsafeRunSync()}")

    def try2IO[A](aTry: Try[A]): IO[A] =
      aTry match
        case Failure(exception) =>
          IO.raiseError(exception)
        case Success(value) =>
          IO(value)

    println(s"try2IO(Success): ${try2IO(Try(22)).unsafeRunSync()}")
    println(s"try2IO(Failure): ${try2IO(Try(1 / 0)).attempt.unsafeRunSync()}")

    def try2IOAlternative[A](aTry: Try[A]): IO[A] = IO.fromTry(aTry)

    println(s"try2IOAlternative(Success): ${try2IOAlternative(Try(22)).unsafeRunSync()}")
    println(s"try2IOAlternative(Failure): ${try2IOAlternative(Try(1 / 0)).attempt.unsafeRunSync()}")

    def either2IO[A](anEither: Either[Throwable, A]): IO[A] =
      anEither match
        case Left(exception) => IO.raiseError(exception)
        case Right(value) => IO(value)

    println(s"either2IO(Right): ${either2IO(222.asRight).unsafeRunSync()}")
    println(s"either2IO( Left): ${either2IO(new RuntimeException("A proper failure").asLeft).attempt.unsafeRunSync()}")

    def either2IOAlternative[A](anEither: Either[Throwable, A]): IO[A] = IO.fromEither(anEither)

    println(s"either2IOAlternative(Right): ${either2IOAlternative(222.asRight).unsafeRunSync()}")
    println(s"either2IOAlternative( Left): ${either2IOAlternative(new RuntimeException("A proper failure").asLeft).attempt.unsafeRunSync()}")

    // 2 - handleError, handleErrorWith
    def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
      io.handleError(handler)

    def handleIOErrorAlternative[A](io: IO[A])(handler: Throwable => A): IO[A] =
      io.redeem(handler, identity)

    println(s"handleIOError(failure): ${handleIOError(aFailure)(_ => -1).unsafeRunSync()}")
    println(s"handleIOErrorAlternative(failure): ${handleIOErrorAlternative(aFailure)(_ => -1).unsafeRunSync()}")

    def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
      io.handleErrorWith(handler)

    def handleIOErrorWithAlternative[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
      io.redeemWith(handler, IO.pure)

    println(s"handleIOErrorWith(failure): ${handleIOErrorWith(aFailure)(_ => IO(-1)).unsafeRunSync()}")
    println(s"handleIOErrorWithAlternative(failure): ${handleIOErrorWithAlternative(aFailure)(_ => IO(-1)).unsafeRunSync()}")
  }
}
