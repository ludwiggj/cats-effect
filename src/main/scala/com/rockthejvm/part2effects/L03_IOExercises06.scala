package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import scala.annotation.tailrec

object L03_IOExercises06 {
  def main(args: Array[String]): Unit = {
    // Exercise 6 - fix stack recursion
    val smallNoOfTerms = 20
    val largeNoOfTerms = 20000
    val largerNoOfTerms = 100000
    val hugeNoOfTerms = 200000000

    // Typical implementation - NOT stack safe
    def sum(n: Int): Int =
      if (n <= 0) 0
      else n + sum(n - 1)

    println(s"sum($smallNoOfTerms) = ${sum(smallNoOfTerms)}")
    // println(s"sum($largeNoOfTerms) = ${sum(largeNoOfTerms)}")    // Stack overflow!

    def sumIO(n: Int): IO[Int] =
      if (n <= 0) IO(0)
      // NOT stack safe as sumIO term evaluated first - also no flatmaps
      else sumIO(n - 1).map(_ + n)

    println(s"sumIO($smallNoOfTerms) = ${sumIO(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO($largeNoOfTerms) = ${sumIO(largeNoOfTerms)}") // Stack overflow - without running

    def sumIO2(n: Int): IO[Int] =
      if (n <= 0) IO(0)
      // NOT stack safe as sumIO2 term evaluated first
      else sumIO2(n - 1).flatMap(acc => IO(acc + n))

    println(s"sumIO2($smallNoOfTerms) = ${sumIO2(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO2($largeNoOfTerms) = ${sumIO2(largeNoOfTerms)}") // Stack overflow - without running

    def sumIO3(n: Int): IO[Int] =
      if (n <= 0) IO(0)
      // NOT stack safe as sumIO3 term evaluated first
      else sumIO3(n - 1).flatMap(acc => IO(n).map(n => n + acc))

    println(s"sumIO3($smallNoOfTerms) = ${sumIO3(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO3($largeNoOfTerms) = ${sumIO3(largeNoOfTerms)}") // Stack overflow - without running

    // This version is stack safe... the recursive call is inside a for comprehension, and it's not the first function
    // inside an IO; in this case it's IO(n)
    def sumIO4(n: BigInt): IO[BigInt] =
      if (n <= 0) IO(0)
      else IO(n).flatMap(n => sumIO4(n - 1).map(acc => n + acc))

    println(s"sumIO4($smallNoOfTerms) = ${sumIO4(smallNoOfTerms).unsafeRunSync()}")
    println(s"sumIO4($largeNoOfTerms) = ${sumIO4(largeNoOfTerms).unsafeRunSync()}")
    println(s"sumIO4($largerNoOfTerms) = ${sumIO4(largerNoOfTerms).unsafeRunSync()}")

    // This version is stack safe... the recursive call is inside a for comprehension, and it's not the first function
    // inside an IO; in this case it's IO(n)
    def sumIO5WithDebug(n: BigInt): IO[BigInt] =
      if (n <= 0) {
        print("n(0) ")
        IO(0)
      } else {
        print(s"n($n) ")
        for {
          n <- IO(n)
          acc <- sumIO5WithDebug(n - 1)
        } yield n + acc
      }

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO5WithDebug($smallNoOfTerms)... ${sumIO5WithDebug(smallNoOfTerms)}")
    println(s"sumIO5WithDebug($smallNoOfTerms) = ${sumIO5WithDebug(smallNoOfTerms).unsafeRunSync()}")

    // Textbook answer
    def sumIO5(n: BigInt): IO[BigInt] =
      if (n <= 0)
        IO(0)
      else
        for {
          n <- IO(n)
          acc <- sumIO5(n - 1)
        } yield n + acc

    println(s"Evaluating sumIO5($hugeNoOfTerms)... ${sumIO5(hugeNoOfTerms)}")
    println(s"sumIO5($largeNoOfTerms) = ${sumIO5(largeNoOfTerms).unsafeRunSync()}")

    // This also works - n is no longer placed inside an IO, but IO.unit performs the same role
    def sumIO6WithDebug(n: BigInt): IO[BigInt] =
      if (n <= 0) {
        print("n(0) ")
        IO(0)
      } else {
        print(s"n($n) ")
        for {
          _ <- IO.unit
          acc <- sumIO6WithDebug(n - 1)
        } yield n + acc
      }

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO6WithDebug($smallNoOfTerms)... ${sumIO6WithDebug(smallNoOfTerms)}")
    println(s"sumIO6WithDebug($smallNoOfTerms) = ${sumIO6WithDebug(smallNoOfTerms).unsafeRunSync()}")

    // Defer also works
    def sumIO7WithDebug(n: BigInt): IO[BigInt] =
      if (n <= 0) {
        print("n(0) ")
        IO(0)
      } else {
        print(s"n($n) ")
        for {
          acc <- IO.defer(sumIO7WithDebug(n - 1))
        } yield n + acc
      }

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO7WithDebug($smallNoOfTerms)... ${sumIO7WithDebug(smallNoOfTerms)}")
    println(s"sumIO7WithDebug($smallNoOfTerms) = ${sumIO7WithDebug(smallNoOfTerms).unsafeRunSync()}")

    def sumIO8(n: BigInt): IO[BigInt] =
      if (n <= 0)
        IO(0)
      else
        for {
          acc <- sumIO8(n - 1) // these terms re the wrong way around
          n <- IO(n)
        } yield n + acc

    // println(s"sumIO8($largeNoOfTerms) = ${sumIO8(largeNoOfTerms)}") // Stack overflow - without running

    // Exception in thread "io-compute-0" java.lang.OutOfMemoryError: Java heap space
    // println("Now running sumIO5(hugeNoOfTerms)")
    // println(s"sumIO5($hugeNoOfTerms) = ${sumIO5(hugeNoOfTerms).unsafeRunSync()}")
  }
}
