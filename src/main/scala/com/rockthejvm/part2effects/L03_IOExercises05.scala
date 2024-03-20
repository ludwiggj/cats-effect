package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import scala.annotation.tailrec

object L03_IOExercises05 {
  // Exercise 4 - convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  // Exercise 5 - discard value from IO, return unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    convert(ioa, ())

  // 'void' method specifically provided to do this
  def asUnit2[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())

  // 'void' method specifically provided to do this
  def asUnit3[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // don't do this

  // 'void' method specifically provided to do this
  def asUnit4[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  def exercise5(): Unit = (for {
    _          <- IO.println("Exercise 5")
    discarded  <- asUnit(IO("two"))
    _          <- IO.println(s"asUnit(IO(\"two\")): $discarded")
    discarded2 <- asUnit2(IO("two"))
    _          <- IO.println(s"asUnit2(IO(\"two\")): $discarded2")
    discarded3 <- asUnit3(IO("two"))
    _          <- IO.println(s"asUnit3(IO(\"two\")): $discarded3")
    discarded4 <- asUnit4(IO("two"))
    _          <- IO.println(s"asUnit4(IO(\"two\")): $discarded4")

  } yield ()).unsafeRunSync()

  // Start of main loop
  def main(args: Array[String]): Unit = {
    exercise5()
  }
}
