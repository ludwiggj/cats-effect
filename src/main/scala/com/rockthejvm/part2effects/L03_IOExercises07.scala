package com.rockthejvm.part2effects

import cats.effect.unsafe.implicits.global
import cats.effect.IO
import cats.syntax.all.*
import scala.annotation.tailrec

object L03_IOExercises07 {
  // Exercise 7 - stack safe fibonacci
  def main(args: Array[String]): Unit = {

    // Typical fibonacci implementation - NOT stack safe
    // n
    // 1  2  3  4  5  6
    // 0, 1, 1, 2, 3, 5, 8, 13
    def fibonacciWithDebug(n: Int): BigInt = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        0
      case n if n == 2 =>
        print(s"n(2) ")
        1
      case n =>
        print(s"n($n) ")
        fibonacciWithDebug(n - 2) + fibonacciWithDebug(n - 1)
    }

    println(s"fibonacciWithDebug(5): ${fibonacciWithDebug(5)}")

    def fibonacci(n: Int): BigInt = n match {
      case n if n <= 1 =>
        0
      case n if n == 2 =>
        1
      case n =>
        fibonacci(n - 2) + fibonacci(n - 1)
    }

    // stack overflow on execution
    // println(s"fibonacci(50000): ${fibonacci(50000)}")

    // Straight translation into IO land - NOT stack safe - recursive call is first IO function
    def fibonacciIO(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        IO(0)
      case n if n == 2 =>
        IO(1)
      case n =>
        fibonacciIO(n - 2).flatMap(n_2 => fibonacciIO(n - 1).map(n_1 => n_2 + n_1))
    }

    // stack overflow on evaluation
    // println(s"fibonacciIO(50000): ${fibonacciIO(50000)}")

    // Straight translation into IO land - NOT stack safe - recursive call is first IO function
    def fibonacciIOWithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print(s"n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        fibonacciIOWithDebug(n - 2).flatMap(n_2 => fibonacciIOWithDebug(n - 1).map(n_1 => n_2 + n_1))
    }

    // stack overflow on evaluation - eager evaluation - gets to n(36496)
    // println(s"fibonacciIOWithDebug(50000): ${fibonacciIOWithDebug(50000)}")

    // NOT stack safe - recursive call is first IO function
    def fibonacciIO2WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        for {
          last <- fibonacciIO2WithDebug(n - 1)
          prev <- fibonacciIO2WithDebug(n - 2)
        } yield last + prev
    }

    // stack overflow on evaluation - eager evaluation - gets to n(44339)
    // println(s"fibonacciIO2WithDebug(50000): ${fibonacciIO2WithDebug(50000)}")

    // stack safe -  recursive call is first IO function, but it's also suspended in an IO - first cut
    def fibonacciIO3WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        for {
          last <- IO(fibonacciIO3WithDebug(n - 1)).flatMap(x => x)
          prev <- fibonacciIO3WithDebug(n - 2)
        } yield last + prev
    }

    println(s"Evaluating fibonacciIO3WithDebug(50000)... ${fibonacciIO3WithDebug(50000)}") // stack safe
    println(s"fibonacciIO3WithDebug(10): ${fibonacciIO3WithDebug(10).unsafeRunSync()}") // it works

    // stack safe -  recursive call is first IO function, but it's also suspended in an IO - delay and flatten
    def fibonacciIO4WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        for {
          last <- IO.delay(fibonacciIO4WithDebug(n - 1)).flatten // IO.delay is the same as IO.apply
          prev <- fibonacciIO4WithDebug(n - 2)
        } yield last + prev
    }

    println(s"Evaluating fibonacciIO4WithDebug(50000)... ${fibonacciIO4WithDebug(50000)}") // stack safe
    println(s"fibonacciIO4WithDebug(9): ${fibonacciIO4WithDebug(9).unsafeRunSync()}") // it works

    // stack safe -  recursive call is first IO function, but it's also suspended in an IO - defer
    def fibonacciIO5WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        for {
          last <- IO.defer(fibonacciIO5WithDebug(n - 1)) // suspend an effect inside another effect
          prev <- fibonacciIO5WithDebug(n - 2)
        } yield last + prev
    }

    println(s"Evaluating fibonacciIO5WithDebug(50000)... ${fibonacciIO5WithDebug(50000)}") // stack safe
    println(s"fibonacciIO5WithDebug(8): ${fibonacciIO5WithDebug(8).unsafeRunSync()}") // it works

    // stack safe - recursive call is first IO function, but it's also suspended in an IO - defer further out
    def fibonacciIO6WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        IO.defer {
          for {
            last <- fibonacciIO6WithDebug(n - 1)
            prev <- fibonacciIO6WithDebug(n - 2)
          } yield last + prev
        }
    }

    println(s"Evaluating fibonacciIO6WithDebug(50000)... ${fibonacciIO6WithDebug(50000)}") // stack safe
    println(s"fibonacciIO6WithDebug(7): ${fibonacciIO6WithDebug(7).unsafeRunSync()}") // it works

    // stack safe - recursive call is first IO function, but it's also suspended in an IO - defer furthest out
    def fibonacciIO7WithDebug(n: Int): IO[BigInt] = IO.defer {
      n match {
        case n if n <= 1 =>
          print(s"n($n) ")
          IO(0)
        case n if n == 2 =>
          print("n(2) ")
          IO(1)
        case n =>
          print(s"n($n) ")
          for {
            last <- fibonacciIO7WithDebug(n - 1)
            prev <- fibonacciIO7WithDebug(n - 2)
          } yield last + prev
      }
    }

    println(s"Evaluating fibonacciIO7WithDebug(50000)... ${fibonacciIO7WithDebug(50000)}") // stack safe
    println(s"fibonacciIO7WithDebug(6): ${fibonacciIO7WithDebug(6).unsafeRunSync()}") // it works

    // stack safe - recursive call is first IO function, but it's preceded by function suspended in an IO
    def fibonacciIO8WithDebug(n: Int): IO[BigInt] = n match {
      case n if n <= 1 =>
        print(s"n($n) ")
        IO(0)
      case n if n == 2 =>
        print("n(2) ")
        IO(1)
      case n =>
        print(s"n($n) ")
        for {
          _ <- IO.unit
          last <- fibonacciIO8WithDebug(n - 1)
          prev <- fibonacciIO8WithDebug(n - 2)
        } yield last + prev
    }

    println(s"Evaluating fibonacciIO8WithDebug(50000)... ${fibonacciIO8WithDebug(50000)}") // stack safe
    println(s"fibonacciIO8WithDebug(5): ${fibonacciIO8WithDebug(5).unsafeRunSync()}") // it works

    // Similar thing is explained here:
    // https://stackoverflow.com/questions/65518551/what-does-cats-effects-io-suspend-function-really-do

    // eager as recursive call not suspended in IO, but won't blow stack due to tail recursion
    @tailrec
    def fibWithDebug(n: Int, a: BigInt, b: BigInt): IO[BigInt] =
      if (n > 0) {
        print(s"n($n) ")
        fibWithDebug(n - 1, b, a + b)
      }
      else {
        print(s"n(0) a($a) ")
        IO.pure(a)
      }

    println(s"Evaluating fibWithDebug(5, 0, 1): ${fibWithDebug(5, 0, 1)}") // evaluated eagerly, but ok as it's tail recursive
    println(s"fibWithDebug(5, 0, 1): ${fibWithDebug(5, 0, 1).unsafeRunSync()}")

    // eager as recursive call not suspended in IO, but won't blow stack due to tail recursion
    @tailrec
    def fib(n: Int, a: BigInt, b: BigInt): IO[BigInt] =
      if (n > 0) {
        fib(n - 1, b, a + b)
      }
      else {
        IO.pure(a)
      }

    println(s"Evaluating fib(50000, 0, 1): ${fib(50000, 0, 1)}") // evaluated eagerly, but ok as it's tail recursive

    // lazy as recursive call preceded by function suspended in IO - no longer tail recursive
    def fib2WithDebug(n: Int, a: BigInt, b: BigInt): IO[BigInt] =
      if (n > 0) {
        for {
          _ <- IO.print(s"n($n) ")
          res <- fib2WithDebug(n - 1, b, a + b)
        } yield res
      } else {
        print(s"n(0) a($a) ")
        IO.pure(a)
      }

    println(s"Evaluating fib2WithDebug(5, 0, 1): ${fib2WithDebug(5, 0, 1)}") // evaluated lazily
    println(s"fib2WithDebug(5, 0, 1): ${fib2WithDebug(5, 0, 1).unsafeRunSync()}")

    val list1to5 = (1 to 5).toList

    println("Traverse = lazy evaluation")
    // fibWithDebug is eager, so fibWithDebug(1, 0, 1) evaluated
    println(s"list1to5.traverse(fibWithDebug(_, 0, 1)): ${list1to5.traverse(fibWithDebug(_, 0, 1))}")
    // fib2WithDebug is lazy, so nothing evaluated
    println(s"list1to5.traverse(fib2WithDebug(_, 0, 1)): ${list1to5.traverse(fib2WithDebug(_, 0, 1))}")

    println("map ... sequence = eager evaulation")
    // fibWithDebug is eager, so all terms evaluated
    println(s"list1to5.map(i => fibWithDebug(i, 0, 1)).sequence: ${list1to5.map(i => fibWithDebug(i, 0, 1)).sequence}")
    // fib2WithDebug is lazy, so nothing evaluated
    println(s"list1to5.map(i => fib2WithDebug(i, 0, 1)).sequence: ${list1to5.map(i => fib2WithDebug(i, 0, 1)).sequence}")
    // TODO buggy expansion? hmm....
    println(s"list1to5.map(_ => fib2WithDebug(_, 0, 1)).sequence (buggy expansion?): ${list1to5.map(_ => fib2WithDebug(_, 0, 1)).sequence}")

    // =========================================
    def forumPost3(): Unit = {
      // I didn’t understand the explanation as to why the recursive fibonacci calls need to wrap the IOs in another
      // layer of IOs? My exercise implementation:
      def fibonacciIO(n: Int): IO[BigInt] = n match {
        case n if n <= 1 => IO(0)
        case n if n == 2 => IO(1)
        case n => for {
          last <- fibonacciIO(n - 1)
          prev <- fibonacciIO(n - 2)
        } yield last + prev
      }

      // The reason given was to avoid stack issues, but this version also ran without any stack issues.

      // Answer(Dawid Łakomy):

      // It indeed causes stack overflow. Try with n=1000000000. Another layer of IO guarantees lazy evaluation.
      // Without this another layer the recursion is eager.

      // I’ve played with both sumIO(5) and fibonacciIO, and I can see that both of the implementations you give in the
      // “IO: Exercises” lecture are stack safe. What still puzzles me is why the recursive call in sumIO does not need
      // to be wrapped in an IO, whereas the recursive calls in fibonacciIO do need to be wrapped in IOs. At 19m 30s in
      // the lecture you mention that sumIO is stack safe because it is based on a for comprehension implemented via a
      // flatMap chain, which has previously been shown to be stack safe. However, fibonacciIO is based on exactly the
      // same structure, with just an extra flatMap call. When you describe fibonacciIO at 22m 10s, you mention the
      // wrapping of the recursive calls in an IO is critical to avoid stack overflow, but don’t really explain why that
      // aspect of fibonacciIO’s behaviour changes from sumIO, even though it is still using a flatMap chain.
      // I’m also wondering what the general approach should be. Do I wrap every recursive call in such functions in an
      // IO.defer? Or do I test it for eager evaluation first, and use that to determine whether to use IO.defer? I know
      // that adding an IO.defer into sumIO still works, but do I pay any penalty for adding an IO.defer to a function
      // when it’s not strictly needed?

      // Answer (Daniel):

      // The rule of thumb I follow is that recursive calls need to be inside some later IOs (i.e. under a flatMap).
      // However, fibonacciIO is based on exactly the same structure, with just an extra flatMap call.
      // That extra flatMap call makes all the difference. When you write

      // for {
      //   a <- fibo(n-1)
      //   b <- fibo(n-2)
      // } yield a + b
      //
      // the fibo(n-1) will crash, because it calls itself recursively. When you write
      //
      // for {
      //   a <- IO.defer(fibo(n-1))
      //   b <- fibo(n-2)
      // }

      // the first recursive call (the dangerous one) is delayed. Buried under another IO. Which means that when the
      // function is evaluated, there's just an IO chain that will then start to unpack fibo(n-1), then fibo(n-2), etc
      // in a stack-safe way. It's as if you said

      // for {
      //   _ <- IO.unit // only used to block eager recursion
      //   a <- fibo(n-1)
      //   b <- fibo(n-2)
      // } yield a + b
    }
  }
}
