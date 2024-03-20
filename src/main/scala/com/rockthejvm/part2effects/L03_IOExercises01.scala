package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

object L03_IOExercises01 {
  // Exercise 1 - sequence two IOs and take result of LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen (eager) - same implementation as above

  def sequenceTakeLast3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen with by-name call (same implementation as above, but io passed by name
               // i.e. lazy, more suitable for recursion, as per >> comments)

  def sequenceTakeLast4[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    for {
      _ <- ioa
      b <- iob
    } yield b


  def exercise1(): Unit = (
    for {
      _ <- IO.println("Exercise 1")
      last <- sequenceTakeLast(IO("first"), IO("last1"))
      _ <- IO.println(s"sequenceTakeLast1: $last")
      last2 <- sequenceTakeLast2(IO("first"), IO("last2"))
      _ <- IO.println(s"sequenceTakeLast2: $last2")
      last3 <- sequenceTakeLast3(IO("first"), IO("last3"))
      _ <- IO.println(s"sequenceTakeLast3: $last3")
      last4 <- sequenceTakeLast4(IO("first"), IO("last4"))
      _ <- IO.println(s"sequenceTakeLast4: $last4")
    } yield ()).unsafeRunSync()

  // Start of main loop
  def main(args: Array[String]): Unit = {
    exercise1()
  }
}
