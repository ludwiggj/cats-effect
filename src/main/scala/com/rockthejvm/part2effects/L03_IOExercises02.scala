package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

object L03_IOExercises02 {
  // Exercise 2 - sequence two IOs and take result of FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  def exercise2(): Unit = (
    for {
      _ <- IO.println("Exercise 2")
      first <- sequenceTakeFirst(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeFirst: $first")
      first2 <- sequenceTakeFirst2(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeFirst2: $first2")
    } yield ()).unsafeRunSync()

  // Start of main loop
  def main(args: Array[String]): Unit = {
    exercise2()
  }
}
