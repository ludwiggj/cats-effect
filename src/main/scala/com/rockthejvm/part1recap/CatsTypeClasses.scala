package com.rockthejvm.part1recap

// - applicative
// - functor
// - flatMap
// - monad
// - apply
// - applicativeError / monadError
// - traverse

object CatsTypeClasses {
  // (1) functor - "mappable"
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor

  val listFunctor = Functor[List]

  // generalise mapping APIs
  def increment[F[_]](container: F[Int])(using functor: Functor[F]):F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor.* // map extension method

  def incrementV2[F[_]: Functor](container: F[Int]):F[Int] =
    container.map(_ + 1)

  // (2) applicative - the ability to wrap types via pure
  trait MyApplicative[F[_]] extends Functor[F] {
    def pure[A](v: A): F[A]
  }

  import cats.Applicative

  val applicativeList = Applicative[List]

  import cats.syntax.applicative.* // pure extension method

  // (3) flatMap - ability to chain multiple wrapper computations
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap

  val flatMapList = FlatMap[List]

  import cats.syntax.flatMap.* // flatMap extension method

  // Generally speaking, flatMap isn't used explicitly
  def crossProductAlt[F[_], A, B](fa: F[A], fb: F[B])(using flatMap: FlatMap[F]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b))) // map available via Flatmap

  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // (4) Monad = applicative + flatmap
  trait MyMonad[F[_]] extends MyFlatMap[F] with MyApplicative[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad

  val monadList = Monad[List]

  def crossProduct2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // (5) MyApplicativeError
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]

  val appErrorEither = ApplicativeError[ErrorOr, String] // Arguments are WrapperType, ErrorType

  val desirable: ErrorOr[Int] = appErrorEither.pure(9)
  val undesirable: ErrorOr[Int] = appErrorEither.raiseError[Int]("Oh no")

  import cats.syntax.applicativeError.* // raiseError extension method

  // Can be used for any types for which given ApplicativeError is in scope
  val undesirable2: ErrorOr[Int] = "Nope".raiseError[ErrorOr, Int]
  val undesirable3: ErrorOr[Double] = "Nope".raiseError[ErrorOr, Double]

  // (6) MyMonadError
  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with MyMonad[F]

  import cats.MonadError

  val monadErrorEither = MonadError[ErrorOr, String] // Arguments are WrapperType, ErrorType

  // (7) traverse

  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }



  def main(args: Array[String]): Unit = {
    println(increment(List(1, 2, 3)))
    println(incrementV2(Option(4)))

    println(applicativeList.pure(43))
    println(43.pure[List]) // will wrap 43 (or any other type) into List, as long as Applicative[List] is in scope

    println(crossProduct(List(1, 2), List(3, 4, 5)))

    println(crossProduct2(List(1, 2), List(3, 4, 5)))

    println(desirable)
    println(undesirable)
    println(undesirable2)
    println(undesirable3)

    // traverse use case - turn nested wrappers inside out
    val listOfOptions = List(Some(1), Some(2), Some(3))

    import cats.Traverse
    val listTraverse = Traverse[List]
    val optionList = listTraverse.traverse(List(1, 2, 3))(Option.apply)
    println(optionList)

    import cats.syntax.traverse.*

    val optionList2 = List(1, 2, 3).traverse(Option.apply)
    println(optionList2)
  }
}
