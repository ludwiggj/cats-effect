package com.rockthejvm.part2effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import scala.annotation.tailrec

object L03_IOExercises03 {
  // Start of main loop
  def main(args: Array[String]): Unit = {
    // Exercise 3 - repeat an IO effect forever
    // This function runs io and then runs it again, and again, and again
    // This is stack safe

    // It creates a data structure using 'final case class FlatMap[E, +A]'; it's actually infinite linked list of
    // IO chains. It is only evaluated when unsafeRunSync is called at the end of the world, using a
    // tail-recursive algorithm
    def forever[A](io: IO[A]): IO[A] = {
      println("Evaluating forever!")
      io.flatMap(_ => forever(io))
    }

    def testForever = forever(IO {
      println("Running forever!")
      Thread.sleep(250)
    })

    // testForever - lazy evaluation
    // Prints "Evaluating forever!" one time when declared
    // *> testForever

    // Repeats i.e. evaluated and executed in lock-step
    //   Evaluating forever!
    //   Running forever!
    // *> testForever.unsafeRunSync()

    // >> is lazy - so stack safe
    // It creates a data structure lazily using 'final case class FlatMap[E, +A]'; it's actually infinite linked list of
    // IO chains. It is only evaluated when unsafeRunSync is called at the end of the world, using a  tail-recursive
    // algorithm
    def forever_v2[A](io: IO[A]): IO[A] = {
      println("Evaluating forever2!")
      io >> forever_v2(io) // same implementation as previous one
    }

    def testForever_v2 =
      forever_v2(IO {
        println("Running forever2!")
        Thread.sleep(250)
      })

    // testForever2 - lazy evaluation
    // *> testForever_v2
    // *> testForever_v2.unsafeRunSync()

    // *> is eager, so causes stack overflow
    // Gets stuck in an evaluation loop
    def forever_v3[A](io: IO[A]): IO[A] = {
      println("Evaluating forever3!")
      io *> forever_v3(io)
    }

    def testForever_v3 =
      forever_v3(IO { // causes stack overflow, even without unsafeRunSync
        println("foreverEager3!")
        Thread.sleep(250)
      })

    // testForever3 - eager evaluation
    // *> testForever_v3

    // This is stack safe
    def forever_v4[A](io: IO[A]): IO[A] = {
      println("Evaluating forever4!")
      io.foreverM
    }

    def testForever_v4 =
      forever_v4(IO {
        println("Running forever4!")
        Thread.sleep(250)
      })

    // testForever4 - lazy evaluation
    // *> testForever_v4
    // *> testForever_v4.unsafeRunSync()

    // This is stack safe
    def forever_v5[A](io: IO[A]): IO[A] = {
      println("Evaluating forever5!")
      io.flatMap(_ => forever_v5(io)).map(identity)
    }

    def testForever_v5 =
      forever_v5(IO {
        println("Running forever5!")
        Thread.sleep(250)
      })

    // testForever5 - lazy evaluation
    // *> testForever_v5
    // *> testForever_v5.unsafeRunSync()

    // This is stack safe
    def forever_v6[A](io: IO[A]): IO[A] = {
      println("Evaluating forever6!")
      for {
        _ <- io
        next <- forever_v6(io)
      } yield next
    }

    def testForever_v6 =
      forever_v6(IO {
        println("Running forever6!")
        Thread.sleep(250)
      })

    // testForever6 - lazy evaluation
    // *> testForever_v6
    // *> testForever_v6.unsafeRunSync()

    // In this case, a is bound to the result of io, it then wraps a in IO and reruns it
    // This is NOT THE SAME! (This was my first attempt at solving the problem)
    // Gets stuck in an evaluation loop
    def forever_v7[A](io: IO[A]): IO[A] = {
      println("Evaluating forever7!")
      io.flatMap(a => forever_v7(IO(a)))
    }

    // Explanation....
    // a is bound to the result of io, it then wraps a in IO and reruns it again and again
    // a is bound to the result of io (println+thread sleep), which is Unit
    // So the actual execution loop is similar, it's what is being run that is different
    // This is why the output looks similar to eager evaluation (in that the inner IO println
    // is not shown), but in this case it doesn't result in a stack overflow error
    def testForever_v7 =
      forever_v7(IO {
        println("Running forever7!")
        Thread.sleep(250)
      })

    // testForever7 - lazy evaluation
    // *> testForever_v7

    // The following prints:
    //   Evaluating forever7!
    //   Running forever7!
    //   Evaluating forever7!
    //   Evaluating forever7!
    //   Evaluating forever7! etc....
    // See definition of testForever_v7 for explanation

    // *> testForever_v7.unsafeRunSync()

    // Forum post about the above issue:

    // It just prints 'Running forever7!' once and then gets stuck. I'm not sure how to think about this. I get that
    // I've suspended the calculation again by wrapping it in another IO, but in the "correct" version it has two
    // references to the same io argument, but each IO execution will be a separate function call. I'm wondering if it's
    // to do with the sequencing of the IO's and where they are evaluated. It'd be great if you could explain why the
    // behaviour differs and how to think about this; I'm trying to find a mental model that works for me.

    // Answer (Dawid Łakomy):

    // io.flatMap(_ => forever(io)) is not an equivalent of io.flatMap(a => myForever(IO(a))). The first one runs io
    // and then runs it again, and again, and again… The second one runs io and then runs Unit wrapped in IO again,
    // and again… (a is bound to the result of io (println+thread sleep), which is Unit).

    // I was also given some debug code to verify this.
    extension[A] (io: IO[A]) {
      def debugWithMap: IO[A] = for {
        a <- io
        t = Thread.currentThread().getName
        _ = println(s"[$t] $a")
      } yield a
    }

    def forever_v7_debug[A](io: IO[A]): IO[A] = {
      println("Evaluating forever7 debug!")
      io.debugWithMap.flatMap(a => forever_v7_debug(IO(a)))
    }

    // Here's the debugged version, showing that the result of evaluating the first IO is unit i.e. ()
    def testForever_v7_debug =
      forever_v7_debug(IO {
        println("Running forever7 debug!")
        Thread.sleep(250)
      })

    // Debugging output (confirms explanation):

    // Evaluating forever7 debug!
    // [io-compute-2] ()
    // Evaluating forever7 debug!
    // [io-compute-2] () etc....

    // *> testForever_v7_debug.unsafeRunSync()

    // Thought I'd try the same debug code on the correct forever implementation
    def forever_debug[A](io: IO[A]): IO[A] = {
      println("Evaluating forever debug!")
      io.debugWithMap.flatMap(_ => forever_debug(io))
    }

    def testForever_debug =
      forever_debug(IO {
        println("Running forever debug!")
        Thread.sleep(250)
      })

    // Prints:
    //   Evaluating forever debug!
    //   Running forever debug!
    //   [io-compute-5] ()
    //   Evaluating forever debug!
    //   Running forever debug!
    //   [io-compute-5] ()
    //   etc...

    // *> testForever_debug.unsafeRunSync()

  }

  // ===========================================

  // Forum post...
  
  // You show the difference between ioa *> iob and ioa >> iob and how it affects the forever function. I understand
  // why the foreverLazy function below is lazy, as the argument to >> is evaluated lazily.

  def foreverLazy[A](io: IO[A]): IO[A] =
    io >> foreverLazy(io)

  // However, looking at the original implementation of forever based on flatMap, I don't understand why that
  // function is lazy i.e.

  def foreverLazy2[A](io: IO[A]): IO[A] = {
    println("lazeee foreverLazy")
    io.flatMap(_ => foreverLazy2(io))
  }

  // (Later note: Daniel explains this in the lecture - it creates a data structure which is evaluated on calling unsafeRunSync)

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

  // def foreverEager[A](io: IO[A]): IO[A] =
  //    io *> foreverEager(io)

  // becomes....

  def foreverEagerStep1[A](io: IO[A]): IO[A] =
    io.productR(foreverEagerStep1(io))

  // Same result, StackOverflowError.
  // Step 2, substitute for productR:
  // def productR[B](that: IO[B]): IO[B] =
  //   flatMap(_ => that)

  // Later note: This implementation also matches that of foreverLazy2
  def foreverEagerStep2[A](io: IO[A]): IO[A] =
    io.flatMap(_ => foreverEagerStep2(io))

  // Now it runs correctly. So I've gone from a solution that runs out of stack space to one that doesn't, just by
  // replacing two IO methods by their implementation. Again, if anyone can explain I'd be grateful as I'm finding
  // this very confusing.

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

  // Answer (Dawid Łakomy):

  // I’d say productR is referentially transparent, it just happens that the stack is not infinite. I guess that there
  // is a lot of opinions on what referential transparency really means. Ideally we should be able to replace a
  // function with its definition without any consequences. Computers are not whiteboards though. Let’s consider this
  // example:
  //
  // def longRunning = {
  //  // assume it takes one second
  //  // it's not an easy task
  //  42
  // }
  //
  // val foo = longRunning
  //
  // if I replace foo with longRunning here
  // the program behavior won't be the same
  // cuz an additional second will pass;
  // both foo and longRunning are referentially
  // transparent
  //
  // val bar = foo
  //
  // See the comment above the bar value. I’d say that everything above is referentially transparent (no side effects,
  // deterministic results and so on); however if we replace foo with longRunning, as described, the whole program will
  // take more time to execute (longRunning will run twice). Does it break referential transparency anywhere? I wouldn’t
  // say so (again: same deterministic results, no side effects). It’s just how eagerly evaluated languages work, we
  // have to take that into account (even in lazily evaluated languages like Haskell, where similar problems can occur
  // elsewhere). Referential transparency as a mental model is one thing, what computers can handle is the other :wink:
}
