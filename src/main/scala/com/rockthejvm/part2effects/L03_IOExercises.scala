package com.rockthejvm.part2effects

import cats.effect.unsafe.implicits.global
import cats.effect.IO
import cats.syntax.all.*
import scala.annotation.tailrec

object L03_IOExercises {
  // Exercise 1 - sequence two IOs and take result of LAST one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  def sequenceTakeLast2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // andThen (eager)

  def sequenceTakeLast3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // andThen lazy i.e. with by-name call (more suitable for recursion, as per >> comments)

  // Exercise 2 - sequence two IOs and take result of FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirst2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  // Exercise 3 - repeat an IO effect forever
  // This function runs io and then runs it again, and again, and again
  def foreverLazy[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazy")
    io.flatMap(_ => foreverLazy(io))
  }

  def foreverLazyMapIdentity[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazyMapIdentity")
    io.flatMap(_ => foreverLazyMapIdentity(io)).map(identity)
  }

  def foreverLazyFor[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazyFor")
    for {
      _ <- io
      next <- foreverLazyFor(io)
    } yield next
  }

  def foreverLazy2[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazy2")
    io
  } >> foreverLazy2(io)

  def foreverLazy3[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazy3")
    io.foreverM
  }

  def foreverEager[A](io: IO[A]): IO[A] = {
    println("eager foreverEager")
    io
  } *> foreverEager(io)

  // foreverLazy runs io and then runs it again, and again, and again
  // In foreverLazyBroken, a is bound to the result of io, it then wraps a in IO and reruns it again and again
  // This is NOT THE SAME!
  def foreverLazyBroken[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazyBroken")
    io.flatMap(a => foreverLazyBroken(IO(a)))
  }

  // Exercise 4 - convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  // 'as' method specifically provided to do this
  def convert2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  // Exercise 5 - discard value from IO, return unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    convert(ioa, ())

  // 'void' method specifically provided to do this
  def asUnit2[A](ioa: IO[A]): IO[Unit] =
    ioa.void

  // Exercise 6 - fix stack recursion
  // Typical implementation - NOT stack safe
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    // NOT stack safe as sumIO term evaluated first
    else sumIO(n - 1).map(_ + n)

  def sumIO2(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    // NOT stack safe as sumIO2 term evaluated first
    else sumIO2(n - 1).flatMap(acc => IO(acc + n))

  def sumIO3(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    // NOT stack safe as sumIO3 term evaluated first
    else sumIO3(n - 1).flatMap(acc => IO(n).map(n => n + acc))

  // This version is stack safe... the recursive call is inside a for comprehension, and it's not the first function
  // inside an IO; in this case it's IO(n)
  def sumIO4(n: BigInt): IO[BigInt] =
    if (n <= 0) IO(0)
    else IO(n).flatMap(n => sumIO4(n - 1).map(acc => n + acc))

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

  def sumIO5(n: BigInt): IO[BigInt] =
    if (n <= 0)
      IO(0)
    else
      for {
        n <- IO(n)
        acc <- sumIO5(n - 1)
      } yield n + acc

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

  // Exercise 7 - stack safe fibonacci

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

  def fibonacci(n: Int): BigInt = n match {
    case n if n <= 1 =>
      0
    case n if n == 2 =>
      1
    case n =>
      fibonacci(n - 2) + fibonacci(n - 1)
  }

  // Straight translation into IO land - NOT stack safe - recursive call is first IO function
  def fibonacciIO(n: Int): IO[BigInt] = n match {
    case n if n <= 1 =>
      IO(0)
    case n if n == 2 =>
      IO(1)
    case n =>
      fibonacciIO(n - 2).flatMap(n_2 => fibonacciIO(n - 1).map(n_1 => n_2 + n_1))
  }

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
        last <- IO.delay(fibonacciIO4WithDebug(n - 1)).flatten
        prev <- fibonacciIO4WithDebug(n - 2)
      } yield last + prev
  }

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
        last <- IO.defer(fibonacciIO5WithDebug(n - 1))
        prev <- fibonacciIO5WithDebug(n - 2)
      } yield last + prev
  }

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

  // eager as recursive call not suspended in IO, but won't blow stack due to tail recursion
  @tailrec
  def fib(n: Int, a: BigInt, b: BigInt): IO[BigInt] =
    if (n > 0) {
      fib(n - 1, b, a + b)
    }
    else {
      IO.pure(a)
    }

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

  def exercise1(): Unit = (
    for {
      _ <- IO.println("Exercise 1")
      last <- sequenceTakeLast(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeLast: $last")
      last2 <- sequenceTakeLast2(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeLast2: $last2")
      last3 <- sequenceTakeLast3(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeLast3: $last3")
    } yield ()).unsafeRunSync()

  def exercise2(): Unit = (
    for {
      _ <- IO.println("Exercise 2")
      first <- sequenceTakeFirst(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeFirst: $first")
      first2 <- sequenceTakeFirst2(IO("first"), IO("last"))
      _ <- IO.println(s"sequenceTakeFirst2: $first2")
    } yield ()).unsafeRunSync()

  def exercise3(): Unit = {
    def exercise3ForeverLazy: IO[Unit] =
      foreverLazy(IO {
        println("ForeverLazy!")
        Thread.sleep(250)
      })

    def exercise3ForeverLazyMapIdentity: IO[Unit] =
      foreverLazyMapIdentity(IO {
        println("ForeverLazyMapIdentity!")
        Thread.sleep(250)
      })

    def exercise3ForeverLazyFor: IO[Unit] =
      foreverLazyFor(IO {
        println("ForeverLazyMapFor!")
        Thread.sleep(250)
      })

    def exercise3ForeverLazy2: IO[Unit] =
      foreverLazy2(IO {
        println("ForeverLazy2!")
        Thread.sleep(250)
      })

    def exercise3ForeverLazy3: IO[Unit] =
      foreverLazy3(IO {
        println("ForeverLazy3!")
        Thread.sleep(250)
      })

    def exercise3ForeverEager: IO[Unit] =
      foreverEager(IO {
        println("foreverEager!")
        Thread.sleep(250)
      })

    // The correct implementation:
    //   def foreverLazy[A](io: IO[A]): IO[A] =
    //     io.flatMap(_ => foreverLazy(io))
    // runs io and then runs it again, and again, and again.

    // In this implementation:
    //   def foreverLazyBroken[A](io: IO[A]): IO[A] =
    //     io.flatMap(a => foreverLazyBroken(IO(a)))
    // a is bound to the result of io, it then wraps a in IO and reruns it again and again
    // a is bound to the result of io (println+thread sleep), which is Unit
    // So the actual execution loop is similar, it's what is being run that is different
    // This is why the output looks similar to eager evaluation (in that the inner IO println
    // is not shown), but in this case it doesn't result in a stack overflow error
    def exercise3ForeverLazyBroken: IO[Unit] =
      foreverLazyBroken(IO {
        println("foreverLazyBroken!")
        Thread.sleep(250)
      })

    // When I asked questions about foreverLazyBroken on the Rock the JVM slack forum, I was also given some debug
    // code to verify this.
    extension[A] (io: IO[A]) {
      def debugWithMap: IO[A] = for {
        a <- io // Map operation on io
        t = Thread.currentThread().getName
        _ = println(s"[$t] $a")
      } yield a

      def debugWithFlatMap: IO[A] = for {
        a <- io // FlatMap operation on io
        _ <- io // Map operation on io
        t = Thread.currentThread().getName
        _ = println(s"[$t] $a")
      } yield a
    }

    def foreverLazyBrokenDebugWithMap[A](io: IO[A]): IO[A] =
      io.debugWithMap.flatMap(a => foreverLazyBrokenDebugWithMap(IO(a)))

    def exercise3ForeverLazyBrokenDebugWithMap: IO[Unit] =
      foreverLazyBrokenDebugWithMap(IO {
        println("foreverLazyBrokenDebugWithMap!")
        Thread.sleep(250)
      })

    def foreverLazyDebugWithMap[A](io: IO[A]): IO[A] =
      io.debugWithMap.flatMap(_ => foreverLazyDebugWithMap(io))

    def exercise3ForeverLazyDebugWithMap: IO[Unit] =
      foreverLazyDebugWithMap(IO {
        println("foreverLazyDebugWithMap!")
        Thread.sleep(250)
      })

    def foreverLazyDebugWithFlatMap[A](io: IO[A]): IO[A] =
      io.debugWithFlatMap.flatMap(_ => foreverLazyDebugWithFlatMap(io))

    def exercise3ForeverLazyDebugWithFlatMap: IO[Unit] =
      foreverLazyDebugWithFlatMap(IO {
        println("foreverLazyDebugWithFlatMap!")
        Thread.sleep(250)
      })

    println("Exercise 3")

    // Prints "lazeee foreverLazy" one time when declared, but not run:
    // exercise3ForeverLazy

    // Prints:
    //    lazeee foreverLazy
    //    ForeverLazy!
    //    lazeee foreverLazy
    //    ForeverLazy!
    // etc... i.e. evaluated and executed in lock-step:
    // exercise3ForeverLazy.unsafeRunSync()

    // Same results as per exercise3ForeverLazy above:
    // exercise3ForeverLazyMapIdentity
    // exercise3ForeverLazyMapIdentity.unsafeRunSync()

    // Same results as per exercise3ForeverLazy above:
    // exercise3ForeverLazyFor
    // exercise3ForeverLazyFor.unsafeRunSync()

    // Same results as per exercise3ForeverLazy above:
    // exercise3ForeverLazy2
    // exercise3ForeverLazy2.unsafeRunSync()

    // Same results as per exercise3ForeverLazy above:
    // exercise3ForeverLazy3
    // exercise3ForeverLazy3.unsafeRunSync()

    // Following results in a stack overflow during evaluation
    // Prints:
    //    eager foreverEager
    //    eager foreverEager
    //    eager foreverEager
    //    Exception in thread "main" java.lang.StackOverflowError
    //    	at java.base/java.lang.System$2.encodeASCII(System.java:2414)
    // exercise3ForeverEager

    // So this has no hope of succeeding, as it results in stack overflow during evaluation:
    // exercise3ForeverEager.unsafeRunSync()

    // Prints "lazeee foreverLazyBroken" one time, when declared but not run:
    // exercise3ForeverLazy
    //
    // When run, prints:
    //    lazeee foreverLazyBroken
    //    lazeee foreverLazyBroken
    // etc...
    // See definition of exercise3ForeverLazyBroken for explanation:
    // exercise3ForeverLazyBroken.unsafeRunSync()

    // When I asked questions about why foreverLazyBroken didn't work on the Rock the JVM slack forum, I was given
    // some debug code to verify this.

    // Prints:
    //    [io-compute-7] ()
    //    [io-compute-7] ()
    //    [io-compute-7] ()
    // etc...
    // Just shows that result of IO is ():
    // exercise3ForeverLazyBrokenDebugWithMap.unsafeRunSync()

    // Thought I'd try the same debug code on the correct foreverLazy implementation:

    // Prints:
    //     [io-compute-5] ()
    //     foreverLazyDebugWithMap!
    //     [io-compute-5] ()
    //     foreverLazyDebugWithMap!
    // etc...
    // I thought it would output foreverLazyDebugWithMap! twice per loop, since the IO is evaluated via io.debugWithMap:
    // exercise3ForeverLazyDebugWithMap.unsafeRunSync()

    // Time to investigate!

    // Prints:
    //    foreverLazyDebugWithFlatMap!
    //    foreverLazyDebugWithFlatMap!
    //    [io-compute-4] ()
    //    foreverLazyDebugWithFlatMap!
    //    foreverLazyDebugWithFlatMap!
    //    [io-compute-4] ()
    // etc...
    // So by forcing the debug statement to use flatMap, this includes the result of running the IO in the overall
    // result, and it outputs as expected. This is a key difference between IO.map and IO.flatMap:
    // exercise3ForeverLazyDebugWithFlatMap.unsafeRunSync()
  }

  def exercise4(): Unit = (for {
    _ <- IO.println("Exercise 4")
    converted <- convert(IO("two"), 2)
    _ <- IO.println(s"convert(IO(\"two\"), 2): $converted")
    converted2 <- convert2(IO("two"), 2)
    _ <- IO.println(s"convert2(IO(\"two\"), 2): $converted2")
  } yield ()).unsafeRunSync()

  def exercise5(): Unit = (for {
    _ <- IO.println("Exercise 5")
    discarded <- asUnit(IO("two"))
    _ <- IO.println(s"asUnit(IO(\"two\")): $discarded")
    discarded2 <- asUnit2(IO("two"))
    _ <- IO.println(s"asUnit2(IO(\"two\")): $discarded2")
  } yield ()).unsafeRunSync()

  def exercise6(): Unit = {
    println("Exercise 6")
    val smallNoOfTerms = 20
    val largeNoOfTerms = 20000
    val largerNoOfTerms = 100000
    val hugeNoOfTerms = 200000000

    println(s"sum($smallNoOfTerms) = ${sum(smallNoOfTerms)}")
    // println(s"sum($largeNoOfTerms) = ${sum(largeNoOfTerms)}")    // Stack overflow!

    println(s"sumIO($smallNoOfTerms) = ${sumIO(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO($largeNoOfTerms) = ${sumIO(largeNoOfTerms)}") // Stack overflow - without running

    println(s"sumIO2($smallNoOfTerms) = ${sumIO2(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO2($largeNoOfTerms) = ${sumIO2(largeNoOfTerms)}") // Stack overflow - without running

    println(s"sumIO3($smallNoOfTerms) = ${sumIO3(smallNoOfTerms).unsafeRunSync()}")
    // println(s"sumIO3($largeNoOfTerms) = ${sumIO3(largeNoOfTerms)}") // Stack overflow - without running

    println(s"sumIO4($smallNoOfTerms) = ${sumIO4(smallNoOfTerms).unsafeRunSync()}")
    println(s"sumIO4($largeNoOfTerms) = ${sumIO4(largeNoOfTerms).unsafeRunSync()}")
    println(s"sumIO4($largerNoOfTerms) = ${sumIO4(largerNoOfTerms).unsafeRunSync()}")

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO5WithDebug($smallNoOfTerms)... ${sumIO5WithDebug(smallNoOfTerms)}")
    println(s"sumIO5WithDebug($smallNoOfTerms) = ${sumIO5WithDebug(smallNoOfTerms).unsafeRunSync()}")
    println(s"sumIO5($largeNoOfTerms) = ${sumIO5(largeNoOfTerms).unsafeRunSync()}")

    println(s"Evaluating sumIO5($hugeNoOfTerms)... ${sumIO5(hugeNoOfTerms)}")

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO6WithDebug($smallNoOfTerms)... ${sumIO6WithDebug(smallNoOfTerms)}")
    println(s"sumIO6WithDebug($smallNoOfTerms) = ${sumIO6WithDebug(smallNoOfTerms).unsafeRunSync()}")

    // Only prints single term when evaluated but not run
    println(s"Evaluating sumIO7WithDebug($smallNoOfTerms)... ${sumIO7WithDebug(smallNoOfTerms)}")
    println(s"sumIO7WithDebug($smallNoOfTerms) = ${sumIO7WithDebug(smallNoOfTerms).unsafeRunSync()}")

    // Exception in thread "io-compute-0" java.lang.OutOfMemoryError: Java heap space
    // println("Now running sumIO5(hugeNoOfTerms)")
    // println(s"sumIO5($hugeNoOfTerms) = ${sumIO5(hugeNoOfTerms).unsafeRunSync()}")
  }

  def exercise7(): Unit = {
    println("Exercise 7")
    println(s"fibonacciWithDebug(5): ${fibonacciWithDebug(5)}")

    // stack overflow on execution
    // println(s"fibonacci(50000): ${fibonacci(50000)}")

    // println(s"fibonacciIO(50000): ${fibonacciIO(50000)}")                     // stack overflow on evaluation
    // println(s"fibonacciIOWithDebug(50000): ${fibonacciIOWithDebug(50000)}")   // stack overflow on evaluation - eager evaluation
    // println(s"fibonacciIO2WithDebug(50000): ${fibonacciIO2WithDebug(50000)}") // stack overflow on evaluation - eager evaluation

    println(s"Evaluating fibonacciIO3WithDebug(50000)... ${fibonacciIO3WithDebug(50000)}") // stack safe
    println(s"fibonacciIO3WithDebug(10): ${fibonacciIO3WithDebug(10).unsafeRunSync()}") // it works

    println(s"Evaluating fibonacciIO4WithDebug(50000)... ${fibonacciIO4WithDebug(50000)}") // stack safe
    println(s"fibonacciIO4WithDebug(9): ${fibonacciIO4WithDebug(9).unsafeRunSync()}") // it works

    println(s"Evaluating fibonacciIO5WithDebug(50000)... ${fibonacciIO5WithDebug(50000)}") // stack safe
    println(s"fibonacciIO5WithDebug(8): ${fibonacciIO5WithDebug(8).unsafeRunSync()}") // it works

    println(s"Evaluating fibonacciIO6WithDebug(50000)... ${fibonacciIO6WithDebug(50000)}") // stack safe
    println(s"fibonacciIO6WithDebug(7): ${fibonacciIO6WithDebug(7).unsafeRunSync()}") // it works

    println(s"Evaluating fibonacciIO7WithDebug(50000)... ${fibonacciIO7WithDebug(50000)}") // stack safe
    println(s"fibonacciIO7WithDebug(6): ${fibonacciIO7WithDebug(6).unsafeRunSync()}") // it works

    println(s"Evaluating fibonacciIO8WithDebug(50000)... ${fibonacciIO8WithDebug(50000)}") // stack safe
    println(s"fibonacciIO8WithDebug(5): ${fibonacciIO8WithDebug(5).unsafeRunSync()}") // it works

    println(s"Evaluating fibWithDebug(5, 0, 1): ${fibWithDebug(5, 0, 1)}") // evaluated eagerly, but ok as it's tail recursive
    println(s"fibWithDebug(5, 0, 1): ${fibWithDebug(5, 0, 1).unsafeRunSync()}")

    println(s"Evaluating fib(50000, 0, 1): ${fib(50000, 0, 1)}") // evaluated eagerly, but ok as it's tail recursive

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
  }

  def forumPost1(): Unit = {
    // My attempt at implementing forever differed from the solution as follows. Instead of
    def forever[A](io: IO[A]): IO[A] =
      io.flatMap(_ => forever(io))

    // I wrote
    def myForever[A](io: IO[A]): IO[A] =
      io.flatMap(a => myForever(IO(a)))

    // However, when I execute my version:
    import cats.effect.unsafe.implicits.global
    //myForever(IO {
    //  println("forever!")
    //  Thread.sleep(250)
    //}).unsafeRunSync()

    // It just prints forever! once and then gets stuck. I'm not sure how to think about this. I get that I've suspended
    // the calculation again by wrapping it in another IO, but in the "correct" version it has two references to th
    // same io argument, but each IO execution will be a separate function call. I'm wondering if it's to do with the
    // sequencing of the IO's and where they are evaluated. It'd be great if you could explain why the behaviour differs
    // and how to think about this; I'm trying to find a mental model that works for me.

    // Answer (Dawid Łakomy):

    // io.flatMap(_ => forever(io)) is not an equivalent of io.flatMap(a => myForever(IO(a))). The first one runs io
    // and then runs it again, and again, and again… The second one runs io and then runs Unit wrapped in IO again,
    // and again… (a is bound to the result of io (println+thread sleep), which is Unit).

    // You can check this by adding debug:
    //  extension[A] (io: IO[A])
    //    def debug: IO[A] = for {
    //      a <- io
    //      t = Thread.currentThread().getName
    //      _ = println(s"[$t] $a")
    //    } yield a
    //
    //  def myForever[A](io: IO[A]): IO[A] =
    //    io.debug.flatMap(a => myForever(IO(a)))

    // See foreverLazyBroken and exercise3ForeverLazyBroken for details
  }

  def forumPost2(): Unit = {
    // You show the difference between ioa *> iob and ioa >> iob and how it affects the forever function. I understand
    // why the foreverLazy function below is lazy, as the argument to >> is evaluated lazily.

    def foreverLazy[A](io: IO[A]): IO[A] =
      io >> foreverLazy(io)

    // However, looking at the original implementation of forever based on flatMap, I don't understand why that
    // function is lazy i.e.

    def foreverLazy2[A](io: IO[A]): IO[A] = { // Later note: Laziness of this function looks like it can be explained
                                              // with reference to recursive function not being first statement in flatMap
                                              // i.e. see later sumIO and fibonacci discussions
      println("lazeee foreverLazy")
      io.flatMap(_ => foreverLazy2(io))
    }

    // It calls the same underlying IO.flatMap, but there are no lazy parameters. If anything its implementation looks
    // the same as the version of forever based on *> which is eager:
    def foreverEager[A](io: IO[A]): IO[A] =
      io *> foreverEager(io)

    // This is getting weird. So I decided to slowly refactor the foreverEager method, substituting the underlying
    // implementation to see what happened. Starting point:
    //
    // foreverEager(IO {
    //   println("forevs!")
    //   Thread.sleep(250)
    // }).unsafeRunSync()
    //
    // Running this results in StackOverflowError.
    //
    // Step 1, substitute for *>:
    // def *>[B](that: IO[B]): IO[B] =
    //   productR(that)

    def foreverEagerStep1[A](io: IO[A]): IO[A] =
      io.productR(foreverEagerStep1(io))

    // Same result, StackOverflowError.
    // Step 2, substitute for productR:
    // def productR[B](that: IO[B]): IO[B] =
    //   flatMap(_ => that)

    def foreverEagerStep2[A](io: IO[A]): IO[A] =
      io.flatMap(_ => foreverEagerStep2(io))

    // Now it runs correctly. So I've gone from a solution that runs out of stack space to one that doesn't, just by
    // replacing two IO methods by their implementation.
    // Later note: Above implementation also matches that of foreverLazy2

    // Answer (Dawid Łakomy):

    // I'm not an expert, but here are my thoughts, after some longer analysis. Due to eager evaluation in Scala,
    // replacing a method with its implementation can change evaluation strategy (btw. Haskell uses lazy evaluation by
    // default, so things are a bit different when doing so; I've read a great article about that, sadly can't find it
    // now). This difference is visible here. productR receives one argument passed by value (not by name). The value is
    // evaluated eagerly, and given its recursive definition it results in StackOverflow. The flatMap call receives a
    // function, and it's evaluated only when it's called (not eagerly). Naive implementation of flatMap could cause
    // stack overflow (mutual recursion), but this implementation is stack-safe.
    //
    // Thoughts:
    //
    // A question in my mind is whether the *> and productR methods are referentially transparent. On the one hand, I
    // understand that referential transparency is about replacing a function with its result, rather than its
    // implementation. On the other hand, I thought that IO was designed to be referentially transparent in general, so
    // I’m surprised that substituting functions in this way has produced a completely different behaviour.
    //
    // Useful links:
    //   https://stackoverflow.com/questions/28992625/exceptions-and-referential-transparency
    //   https://stackoverflow.com/questions/210835/what-is-referential-transparency
    //
    // I wrote:
    //
    // So does that imply that the productR method is not referentially transparent, as substituting its implementation
    // into foreverEager in step 2 above changes the behaviour of foreverEager from a function that is evaluated eagerly
    // to one which is evaluated lazily? I find that idea interesting, as IO is always presented as a referentially
    // transparent construct.
  }

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

    // I’ve played with both sumIO and fibonacciIO, and I can see that both of the implementations you give in the
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

  // Start of main loop
  def main(args: Array[String]): Unit = {
    exercise1()
    exercise2()
    exercise3()
    exercise4()
    exercise5()
    exercise6()
    exercise7()
    forumPost1()
    forumPost2()
    forumPost3()
  }
}
