package com.rockthejvm.part5polymorphic

import cats.{ApplicativeError, MonadError, Monoid}
import cats.data.Validated

// see https://typelevel.org/cats/typeclasses/applicativemonaderror.html
object TypeLevelApplicativeMonadError {
  /*
  trait ApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](e: E): F[A]

    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    def handleError[A](fa: F[A])(f: E => A): F[A]

    def attempt[A](fa: F[A]): F[Either[E, A]]
    //More functions elided
  }
  */

  // We can start with a less abstract way of performing a function. Here we will divide one number by another.
  def attemptDivide(x: Int, y: Int): Either[String, Int] = {
    if (y == 0) Left("divisor is zero")
    else {
      Right(x / y)
    }
  }

  // We can abstract the Either away to support any other kind of "error" type without having to create multiple
  // functions with different "container" types.

  // The above method summons ApplicativeError to provide behavior representing an error where the end-user,
  // based on type, will get their appropriate response e.g. F[_] could represent Either
  private def attemptDivideApplicativeError[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      // If F[_] is an Either, then ae.raiseError will return Left.
      // If F[_] represents a Validation, then ae.raiseError will return Invalid
      ae.raiseError("Oh dear, an error has occurred")
    else
      ae.pure(x / y)
  }

  private def attemptDivideApplicativeErrorWithMap2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      ae.raiseError("Oh dear, an error has occurred")
    else
      val fa = ae.pure(x)
      val fb = ae.pure(y)
      ae.map2(fa, fb)(_ / _)
  }

  private def attemptDivideApplicativeErrorAbove2[F[_]](x: Int, y: Int)(implicit ae: ApplicativeError[F, String]): F[Int] = {
    if (y == 0)
      ae.raiseError("Bad Math")
    else if (y == 1)
      ae.raiseError("Waste of Time")
    else
      ae.pure(x / y)
  }

  private def handler[F[_]](f: F[Int])(implicit ae: ApplicativeError[F, String]): F[Int] =
    ae.handleError(f) {
      case "Bad Math" => -1
      case "Waste of Time" => -2
      case _ => -3
    }

  // Use monoid to determine error value
  private def handleErrorWith[F[_], A](f: F[A])(implicit ae: ApplicativeError[F, String], m: Monoid[A]): F[A] =
    ae.handleErrorWith(f)(_ => ae.pure(m.empty))

  // handling exceptions
  def parseInt[F[_]](input: String)(implicit ae: ApplicativeError[F, Throwable]): F[Int] =
    try {
      ae.pure(input.toInt)
    } catch {
      case nfe: NumberFormatException => ae.raiseError(nfe)
    }

  def parseIntBetter[F[_]](input: String)(implicit ae: ApplicativeError[F, Throwable]): F[Int] =
    ae.catchOnly[NumberFormatException](input.toInt)

  /*
    Since a Monad extends an Applicative, there is naturally a MonadError that will extend the
    functionality of the ApplicativeError to provide flatMap composition.

    trait MonadError[F[_], E] extends ApplicativeError[F, E] with Monad[F] {
      def ensure[A](fa: F[A])(error: => E)(predicate: A => Boolean): F[A]
      def ensureOr[A](fa: F[A])(error: A => E)(predicate: A => Boolean): F[A]
      def adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A]
      def rethrow[A, EE <: E](fa: F[Either[EE, A]]): F[A]
    }
   */

  // Use Case
  // Given a method that accepts a tuple of coordinates, find the closest city.

  // For this example we will hard-code "Minneapolis, MN" but you can imagine that
  // the method would consult a database or a web service.
  def getCityClosestToCoordinate[F[_]](x: (Int, Int))(implicit ae: ApplicativeError[F, String]): F[String] =
    ae.pure("Minneapolis, MN")

  // getTemperatureByCity; given a city, return its temperature
  def getTemperatureByCity[F[_]](city: String)(implicit ae: ApplicativeError[F, String]): F[Int] =
    ae.pure(78)

  import cats.syntax.all.{toFlatMapOps, toFunctorOps}

  def getTemperatureByCoordinates[F[_]: [F[_]] =>> MonadError[F, String]](x: (Int, Int)): F[Int] =
    if (x._1 < 0 || x._2 < 0)
      MonadError[F, String].raiseError("Invalid Coordinates")
    else  
      for {
        c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureByCity[F](c)
      } yield t

  def getTemperatureByCoordinatesAlternative[F[_]](x: (Int, Int))(implicit me: MonadError[F, String]): F[Int] =
    if (x._1 < 0 || x._2 < 0)
      me.raiseError("Invalid Coordinates")
    else
      for {
        c <- getCityClosestToCoordinate[F](x)
        t <- getTemperatureByCity[F](c)
      } yield t

  def main(args: Array[String]): Unit = {
    println(attemptDivide(4, 2))
    println(attemptDivide(4, 0))

    val a = attemptDivideApplicativeError(30, 0) // Either implicit is automatically available (apparently)
    val b: Either[String, Int] = attemptDivideApplicativeError(10, 10)
    println(a)
    println(b)

    // Either
    type OnError[A] = Either[String, A]
    val c: OnError[Int] = attemptDivideApplicativeError(20, 10)
    val d = attemptDivideApplicativeError[OnError](30, 10)
    println(c)
    println(d)

    println(ApplicativeError[OnError, String].raiseError("Bum"))
    println(ApplicativeError[Option, Unit].raiseError(()))

    // ApplicativeError can support other "error" based types e.g. Validated
    type MyValidated[A] = Validated[String, A]

    val e = attemptDivideApplicativeError[MyValidated](40, 10)
    println(e)

    println(ApplicativeError[MyValidated, String].pure(List(1, 2, 3)))
    println(ApplicativeError[MyValidated, String].raiseError("Oh shoot"))

    // type lambdas - see https://blog.rockthejvm.com/scala-3-type-lambdas/
    val f = attemptDivideApplicativeError[[A] =>> Validated[String, A]](50, 10)
    println(f)
    println(ApplicativeError[[A] =>> Validated[String, A], String].raiseError("Oh shoot again"))

    val g = attemptDivideApplicativeErrorWithMap2[[A] =>> Either[String, A]](60, 10)
    println(g)

    println(handler(attemptDivideApplicativeErrorAbove2(3, 0)))
    println(handler(attemptDivideApplicativeErrorAbove2(3, 1)))
    println(handler(attemptDivideApplicativeErrorAbove2(3, 2)))

    println(handleErrorWith(attemptDivideApplicativeErrorAbove2(3, 0)))
    println(handleErrorWith(attemptDivideApplicativeErrorAbove2(3, 1)))
    println(handleErrorWith(attemptDivideApplicativeErrorAbove2(3, 2)))

    println(parseInt[[A] =>> Either[Throwable, A]]("6"))
    println(parseInt[[A] =>> Either[Throwable, A]]("6"))
    println(parseIntBetter[[A] =>> Either[Throwable, A]]("5"))
    println(parseIntBetter[[A] =>> Either[Throwable, A]]("five"))

    println(getTemperatureByCoordinates((1, 2)))
    println(getTemperatureByCoordinates((-1, 2)))
    
    println(getTemperatureByCoordinatesAlternative((1, 2)))
    println(getTemperatureByCoordinatesAlternative((-1, 2)))
  }
}
