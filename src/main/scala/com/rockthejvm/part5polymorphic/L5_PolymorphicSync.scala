package com.rockthejvm.part5polymorphic

import cats.Defer
import cats.effect.kernel.MonadCancel
import cats.effect.{IO, IOApp, Sync}

import java.io.{BufferedReader, InputStreamReader}

object L5_PolymorphicSync extends IOApp.Simple {

  // delay
  // - ability to "suspend" (delay) a computation (external to CE) in IO
  // - if evaluated, the computation is performed on the CE thread pool
  val aDelayedIO = IO.delay {
    println("I'm an effect")
    42
  }

  // blocking
  // - ability to "suspend" (delay) a computation (external to CE) in IO
  // - if evaluated, the computation is performed on a specific thread pool for blocking computations
  val aBlockingIO = IO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  // synchronous computation embodies the two abilities described above + defer (see below)
  // represented as the typeclass Sync[F]
  // - delay = wrapping any computation in F (an effect managed by Cats Effect)
  // - blocking = a blocking computation, wrapped in F
  
  // Goal - generalise synchronous code for any effect type
  // foreign function interface (FFI) = suspending computations (including side effects) executed in another context
  // i.e. external to Cats Effect

  import cats.syntax.flatMap.*

  // Defer is a standalone (cats core) typeclass that just implements defer
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    // implementations added just so I can test defer and deferTextbook
    def delay[A](thunk: => A): F[A] // "suspension" of a computation - will run on the CE thread pool

    def blocking[A](thunk: => A): F[A] // runs on the blocking thread pool

    // Exercise - defer (Implementation of defer in Sync comes for free as it can be implemented in terms of flatMap and delay) 
    // Defer - definition
    // Suspends a synchronous side effect which produces an IO in IO.
    // This is useful for trampolining (i.e. when the side effect is conceptually the allocation of a stack frame).
    // Any exceptions thrown by the side effect will be caught and sequenced into the IO.
    def dodgyDefer[A](thunk: => F[A]): F[A] = flatMap(thunk)(a => delay(a))

    def textbookDefer[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(fa => fa)

    def textbookDefer_v2[A](thunk: => F[A]): F[A] = flatMap(delay(thunk))(identity)

    def textbookDefer_v3[A](thunk: => F[A]): F[A] = flatten(delay(thunk))
  }

  trait MySyncForTesting {
    def dodgyDefer[F[_], A](thunk: => F[A])(using sync: Sync[F]): F[A] = sync.flatMap(thunk)(a => sync.delay(a))

    def textbookDefer[F[_], A](thunk: => F[A])(using sync: Sync[F]): F[A] = sync.flatMap(sync.delay(thunk))(fa => fa)

    def textbookDefer_v2[F[_], A](thunk: => F[A])(using sync: Sync[F]): F[A] = sync.flatMap(sync.delay(thunk))(identity)

    def textbookDefer_v3[F[_], A](thunk: => F[A])(using sync: Sync[F]): F[A] = sync.flatten(sync.delay(thunk))
  }

  val syncIO = Sync[IO] // given Sync[IO] in scope

  // abilities: pure, map, flatMap, raiseError, uncancelable, +delay/blocking

  val aDelayedIO_v2 = syncIO.delay {
    println("I'm an effect")
    42
  }

  val aBlockingIO_v2 = syncIO.blocking {
    println("loading...")
    Thread.sleep(1000)
    42
  }

  val mySync = new MySyncForTesting {}

  val aDodgilyDeferredIO = mySync.dodgyDefer {
    println("Dodgy")
    IO("Dodgily deferred IO")
  }

  val aTextbookDeferredIO = mySync.textbookDefer {
    println("Textbook")
    IO("Textbook deferred IO")
  }

  val aSecondTextbookDeferredIO = mySync.textbookDefer_v2 {
    println("Textbook 2")
    IO("Textbook 2 deferred IO")
  }

  val aThirdTextbookDeferredIO = mySync.textbookDefer_v3 {
    println("Textbook 3")
    IO("Textbook 3 deferred IO")
  }

  // Exercise - write a polymorphic console
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]

    def readLine(): F[String]
  }

  object Console {
    def apply[F[_]](using sync: Sync[F]): F[Console[F]] = sync.delay(
      new Console[F]:
        override def println[A](a: A): F[Unit] = sync.delay(scala.Console.println(a))

        override def readLine(): F[String] = sync.delay(scala.io.StdIn.readLine())
    )
  }

  // System.in, System.out are java streams
  import cats.syntax.functor.*
  object TextbookConsole {
    def apply[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F]:
        override def println[A](a: A): F[Unit] =
          sync.blocking(out.println(a))
          
          // There's a potential problem if a thread from the blocking pool hangs
          // (or - oh my! - one of the CE threads)
          
          // There's also sync.interruptible(true/false) which attempts to block the thread via thread interrupts in
          // case of cancellation. The flag indicates whether you want the thread interrupt signals to be sent
          // repeatedly (true) or not (false)

        override def readLine(): F[String] =
          val bufferedReader = new BufferedReader(new InputStreamReader(in))
          sync.blocking(bufferedReader.readLine())
    }
  }

  val consoleProgram = for {
    console <- Console[IO]
    line1 <- console.readLine()
    line2 <- console.readLine()
    _ <- console.println(s"L1[$line1] L2[$line2]")
  } yield ()

  val textbookConsoleProgram = for {
    console <- Console[IO]
    _ <- console.println("L1>")
    line1 <- console.readLine()
    _ <- console.println("L2>")
    line2 <- console.readLine()
    _ <- console.println(s"L1[$line1] L2[$line2]")
  } yield ()

  override def run: IO[Unit] = textbookConsoleProgram
}

/*
    ____________          _____________          __________       ______________
   |           |         |            |         |         |      |             |
   | Semigroup |         |  Foldable  |         | Functor |      | Semigroupal |
   |  combine  |         |            |         |   map   |      |   product   |
   |___________|         |____________|         |_________|      |_____________|
        ^                       ^                 ^     ^               ^
        |                       |_________________|     |_______________|
        |                                |                      |
    ____________                   _____________          ______________
   |           |                  |            |         |             |
   |  Monoid   |                  |  Traverse  |         |    Apply    |
   |  empty    |                  |  traverse  |         |     ap      |
   |___________|                  |____________|         |_____________|
                                                               ^
                                                               |
                                                    _________________________
                                                   |                        |
                                            ______________           ______________
                                           |             |          |             |
                                           |   FlatMap   |          | Applicative |
                                           |   flatMap   |          |    pure     |
                                           |_____________|          |_____________|
                                                  ^                      ^  ^
                                                  |______________________|  |__________
                                                               |                      |
                                                           __________         ___________________
                                                          |         |        |                  |
                                                          |  Monad  |        | ApplicativeError |
                                                          |         |        |    raiseError    |
                                                          |         |        |  handleErrorWith |
                                                          |_________|        |__________________|
                                                               ^                     ^
                                                               |_____________________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |  MonadError  |
                                                                  |    ensure    |
                                                                  |______________|
                                                                         |
                                 ____________                      _______________
                                |    Cats   |                     |              |
                                |           |                     |  MonadCancel |
                                |   Defer   |                     |   canceled   |
                                |   defer   |                     | uncancelable |
                                |___________|                     |______________|
                                      ^                                   ^
                                      |                                   |
                                      |        ____________________________
                                      |       |                           |
                                   _______________                _______________
                                  |              |               |              |
                                  |     Sync     |               |     Spawn    |
                                  |    delay     |               |     start    |
                                  |   blocking   |               |              |
                                  |______________|               |______________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |  Concurrent  |
                                                                  |     ref      |
                                                                  |   deferred   |
                                                                  |______________|
                                                                         |
                                                                   _______________
                                                                  |              |
                                                                  |   Temporal   |
                                                                  |     sleep    |
                                                                  |______________|
                                                                  
 */