package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import scala.annotation.tailrec

object L03_IOExercises04 {
  // Exercise 4 - convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  // 'as' method specifically provided to do this
  def convert2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)


  def exercise4(): Unit = (for {
    _          <- IO.println("Exercise 4")
    converted  <- convert(IO("two"), 2)
    _          <- IO.println(s"convert(IO(\"two\"), 2): $converted")
    converted2 <- convert2(IO("two"), 2)
    _          <- IO.println(s"convert2(IO(\"two\"), 2): $converted2")
  } yield ()).unsafeRunSync()

  // Start of main loop
  def main(args: Array[String]): Unit = {
    exercise4()
  }
}
