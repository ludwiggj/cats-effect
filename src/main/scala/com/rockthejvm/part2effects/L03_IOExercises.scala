package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}
import com.rockthejvm.part2effects.L03_IOExercises.sum

import scala.io.StdIn

object L03_IOExercises extends IOApp.Simple {
  // Exercise 1 - sequence two IOs and take result of LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen with by-name call (more suitable for recursion, as per >> comments)

  // Exercise 2 - sequence two IOs and take result of FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // Exercise 3 - repeat an IO effect forever
  def foreverLazyBroken[A](io: IO[A]): IO[A] =
    io.flatMap(a => foreverLazyBroken(IO(a)))

  // Using:
  //  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
  //    MyIO(() => f(unsafeRun()).unsafeRun())

  // MyIO(() => (a => foreverLazyBroken(IO(a)))(io.unsafeRun()).unsafeRun())
  // MyIO(() => (a => foreverLazyBroken(IO(a)))(a).unsafeRun())
  // MyIO(() => (a => foreverLazyBroken(IO(a)))(a).unsafeRun())
  // MyIO(() => foreverLazyBroken(IO(a)).unsafeRun())

  def foreverLazy[A](io: IO[A]): IO[A] =
    io.flatMap(_ => foreverLazy(io))

  // Using:
  //  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
  //    MyIO(() => f(unsafeRun()).unsafeRun())

  // MyIO(() => (_ => foreverLazy(io))(io.unsafeRun()).unsafeRun())
  // MyIO(() => (_ => foreverLazy(io))(a).unsafeRun())
  // MyIO(() => foreverLazy(io).unsafeRun())

  // Using actual definitions

  // def flatMap[B](f: A => IO[B]): IO[B] =
  //   IO.FlatMap(this, f, Tracing.calculateTracingEvent(f.getClass))

  def foreverLazy_v2[A](io: IO[A]): IO[A] =
    io >> foreverLazy_v2(io)

  // Using actual definitions

  // def >>[B](that: => IO[B]): IO[B] =
  //   flatMap(_ => that)

  // io.flatMap(_ => (lazy) foreverLazy_v2(io))

  // def flatMap[B](f: A => IO[B]): IO[B] =
  //   IO.FlatMap(this, f, Tracing.calculateTracingEvent(f.getClass))

  def foreverEager[A](io: IO[A]): IO[A] =
    io *> foreverEager(io)

  def foreverEager2[A](io: IO[A]): IO[A] =
    io.productR(foreverEager2(io))

  // Exercise 4 - convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  // Exercise 5 - discard value from IO, return unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    convert(ioa, ())

  // Exercise 6 - fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  //  def sumIO(n: Int): IO[Int] =
  //    if (n <= 0) IO(0)
  //    else sumIO(n-1).map(_ + n)

  //  def sumIO(n: Int): IO[Int] =
  //    if (n <= 0) IO(0)
  //    else sumIO(n-1).flatMap(acc => IO(acc + n))

  //  def sumIO(n: Int): IO[Int] =
  //    if (n <= 0) IO(0)
  //    else sumIO(n-1).flatMap(acc => IO(n).map(n => n + acc))

  //  def sumIO(n: Int): IO[Int] =
  //    if (n <= 0) IO(0)
  //    else {
  //      val nio = IO(n)
  //      sumIO(n-1).flatMap(acc => nio.map(n => n + acc))
  //    }

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else IO(n).flatMap(n => sumIO(n - 1).map(acc => n + acc))

  def fibonacci(n: Int): BigInt = n match {
    case n if n <= 1 => 0
    case n if n == 2 => 1
    case n => fibonacci(n - 2) + fibonacci(n - 1)
  }

  // n
  // 1  2  3  4  5  6
  // 0, 1, 1, 2, 3, 5, 8, 13

  // Exercise 7 - stack safe fibonacci
  def fibonacciIO(n: Int): IO[BigInt] = n match {
    case n if n <= 1 => IO(0)
    case n if n == 2 => IO(1)
    case n => fibonacciIO(n - 2).flatMap(n_2 => fibonacciIO(n - 1).map(n_1 => n_2 + n_1))
  }

  import cats.implicits.*

  def run = {
    for {
      last <- sequenceTakeLast(IO("first"), IO("last"))
      _ <- IO.println(last)
      last2 <- sequenceTakeLast_v2(IO("first"), IO("last"))
      _ <- IO.println(last2)
      last3 <- sequenceTakeLast_v3(IO("first"), IO("last"))
      _ <- IO.println(last3)
      first <- sequenceTakeFirst(IO("first"), IO("last"))
      _ <- IO.println(first)
      first2 <- sequenceTakeFirst_v2(IO("first"), IO("last"))
      _ <- IO.println(first2)
      converted <- convert(IO("two"), 2)
      _ <- IO.println(converted)
      discarded <- asUnit(IO("two"))
      _ <- IO.println(discarded)
      totes <- sumIO(40000)
      _ <- IO.println(totes)
      fibs = (1 to 20).map(fibonacci)
      _ <- IO.println(fibs)
      fibs2 <- (1 to 20).toList.traverse(fibonacciIO)
      _ <- IO.println(fibs2)
      //      bigFib = fibonacci(75)
      //      _ <- IO.println(bigFib)
      _ <- foreverEager(IO {
        println("forever")
        Thread.sleep(500)
      })
    } yield ()
  }
}

object L03_IOExercises_2 {
  import L03_IOExercises.*

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global

//    foreverLazy(IO {
//      println("forevs!")
//      Thread.sleep(250)
//    }).unsafeRunSync()

//    foreverEager(IO {
//      println("forevs!")
//      Thread.sleep(250)
//    })

    foreverEager2(IO {
      println("forevs!")
      Thread.sleep(250)
    })
  }
}