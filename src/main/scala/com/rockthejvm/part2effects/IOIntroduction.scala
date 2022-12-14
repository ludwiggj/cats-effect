package com.rockthejvm.part2effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {
  // IO
  val ourFirstIO: IO[Int] = IO.pure(42) // arg should not have side effects (it is evaluated eagerly)
  val aDelayedIO: IO[Int] = IO.delay({
    println("I'm producing an integer")
    54
  })

  val shouldNotDoThis: IO[Int] = IO.pure({
    println("I'm producing an integer")
    54
  })

  val aDelayedIO_v2: IO[Int] = IO { // apply = delay
    println("Hey!")
    21
  }

  // map, flatMap
  val improved = ourFirstIO.map(_ * 2)
  val printedImproved = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply.*
  val combined: IO[Int] = (ourFirstIO, improved).mapN(_ + _)

  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // platform

    // the end of the world
    println(aDelayedIO.unsafeRunSync())
    println(smallProgram().unsafeRunSync())
    println(smallProgram_v2().unsafeRunSync())
  }
}
