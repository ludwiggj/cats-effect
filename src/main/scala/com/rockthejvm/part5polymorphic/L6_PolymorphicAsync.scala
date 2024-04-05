package com.rockthejvm.part5polymorphic

import cats.effect.{Async, Concurrent, IO, IOApp, Sync, Temporal}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import com.rockthejvm.utils.general.*

object L6_PolymorphicAsync extends IOApp.Simple {

  // Async - asynchronous computation, suspended in F

  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental methods

    // expose underlying execution context on which all fibers are evaluated by CE (quite low level)
    def executionContext: F[ExecutionContext]

    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]

    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]

    // these can be implemented in terms of the fundamentals
    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(kb => map(pure(cb(kb)))(_ => None))

    def never[A]: F[A] = // never-ending effect
      async_(_ => ())
  }

  val asyncIO = Async[IO] // given Async[IO]

  // pure, map, flatMap, raiseError, uncancelable, sleep, ref, deferred, start, delay, defer, blocking
  val ec = asyncIO.executionContext

  // power: async_ + async: FFI
  // foreign function interface (FFI) = suspending async computations (including side effects) executed in another
  // context i.e. external to Cats Effect
  val threadPool = Executors.newFixedThreadPool(10)

  type Callback[A] = Either[Throwable, A] => Unit

  val asyncMol: IO[Int] = IO.async_ { (cb: Callback[Int]) =>
    // start computation on some other threadpool not managed by CE
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMolComplex: IO[Int] = IO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Option(IO("Cancelled").debug.void)) // <- finaliser in case the computation is cancelled
  }

  val asyncMol_v2: IO[Int] = asyncIO.async_ { (cb: Callback[Int]) =>
    // start computation on some other threadpool not managed by CE
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async MOL")
      cb(Right(42))
    }
  }

  val asyncMolComplex_v2: IO[Int] = asyncIO.async { (cb: Callback[Int]) =>
    IO {
      threadPool.execute { () =>
        println(s"[${Thread.currentThread().getName}] Computing an async MOL")
        cb(Right(42))
      }
    }.as(Option(IO("Cancelled").debug.void)) // <- finaliser in case the computation is cancelled
  }

  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  // async computation on some execution context (not managed by cats effect) 
  val asyncMol_v3: IO[Int] = asyncIO.evalOn(IO(42).debug, myExecutionContext).guarantee(
    IO(threadPool.shutdown())
  )

  // never
  val neverIO = asyncIO.never

  // Exercises
  // 1 - implement never and async_ in terms of async

  // 1.1 Implement never using async
  def never[F[_], A](using async: Async[F]): F[A] = {
    async.async_ { _ => () }
  }

  def neverEver[F[_], A](using async: Async[F]): F[A] = {
    async.async {
      _ => async.pure(Option(async.unit))
    }
  }

  val neverEndingProgram = for {
    _ <- IO("never...").debug
    _ <- never[IO, Int]
    _ <- IO("ending story....").debug
  } yield ()

  val neverEverEndingProgram = for {
    _ <- IO("never... ever...").debug
    _ <- neverEver[IO, Int]
    _ <- IO("ending story....").debug
  } yield ()

  // 1.2 Implement async_ using async
  def async_[F[_], A](k: (Either[Throwable, A] => Unit) => Unit)(using async: Async[F]): F[A] =
    async.async { innerK =>
      async.pure(Option(async.pure(k(innerK))))
    }

  def textBookAsync_[F[_], A](k: (Either[Throwable, A] => Unit) => Unit)(using async: Async[F]): F[A] =
    async.async {
      innerK => async.map(async.pure(k(innerK)))(_ => None) // can map to None as there is no callback
    }

  val async_DoubleMol: IO[Int] = async_ { (cb: Callback[Int]) =>
    // start computation on some other threadpool not managed by CE
    threadPool.execute { () =>
      println(s"[${Thread.currentThread().getName}] Computing an async double MOL")
      cb(Right(42 + 42))
    }
  }

  // 2 - tuple two effects with different requirements
  def firstEffect[F[_] : Concurrent, A](a: A): F[A] = Concurrent[F].pure(a)

  def secondEffect[F[_] : Sync, A](a: A): F[A] = Sync[F].pure(a)

  // Does not compile - Ambiguous implicits

  //  import cats.syntax.flatMap.*
  //  import cats.syntax.functor.*

  //  def tupledEffect[F[_]: Sync: Concurrent, A](a: A) =
  //      for {
  //        first <- firstEffect(a)
  //        second <- secondEffect(a)
  //      } yield (first, second)

  def tupledEffect[F[_], A](a: A)(using concurrent: Concurrent[F], sync: Sync[F]): F[(A, A)] =
    concurrent.flatMap(firstEffect(a)) { first =>
      concurrent.map(secondEffect(a)) { second =>
        (first, second)
      }
    }

  // Other way is to find the weakest typeclass that fulfils both constraints, in this case Async

  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  def textbookTupledEffect[F[_]: Async, A](a: A): F[(A, A)] =
    for {
      first <- firstEffect(a)
      second <- secondEffect(a)
    } yield (first, second)

  override def run: IO[Unit] =
    // asyncMol.debug.void >> IO.delay(threadPool.shutdown())
    // async_DoubleMol.debug.void >> IO.delay(threadPool.shutdown())
    // tupledEffect[IO, Int](16).debug.void
    textbookTupledEffect[IO, Int](32).debug.void
  // neverEverEndingProgram
  // neverEndingProgram
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
                                         ^                               |
                                         |                         _______________
                                         |                        |              |
                                         |                        |  Concurrent  |
                                         |                        |     ref      |
                                         |                        |   deferred   |
                                         |                        |______________|
                                         |                               |
                                         |                         _______________
                                         |                        |              |
                                         |                        |   Temporal   |
                                         |                        |     sleep    |
                                         |                        |______________|
                                         |                               ^
                                         |_______________________________|
                                                       |
                                                 _______________
                                                |              |
                                                |     Async    |
                                                |     async    |
                                                |______________|

 */