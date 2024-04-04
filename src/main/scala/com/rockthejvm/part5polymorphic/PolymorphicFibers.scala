package com.rockthejvm.part5polymorphic

import cats.effect.kernel.Outcome.*
import cats.effect.{Fiber, IO, IOApp, MonadCancel, Outcome, Spawn}
import com.rockthejvm.utils.general.*
import scala.concurrent.duration.*

object PolymorphicFibers extends IOApp.Simple {
  val mol = IO(42)
  val fiber: IO[Fiber[IO, Throwable, Int]] = mol.start

  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]] // creates a fiber

    def never[A]: F[A] // a forever-suspending effect

    def cede: F[Unit] // a "yield" effect
    
    // fundamental racing operation
    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[
      (Outcome[F, E, A], Fiber[F, E, B]),
      (Fiber[F, E, A], Outcome[F, E, B])
    ]]
  }

  // Spawn = create fibers for any effect
  // This is still specific, in terms of Throwable
  // This is actually a concrete type of GenSpawn
  trait MySpawnOrig[F[_]] extends MonadCancel[F, Throwable] {
    // Most important method in the API
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // creates a fiber

    def never[A]: F[A] // a forever-suspending effect

    def cede: F[Unit] // a "yield" effect
  }

  // therefore
  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  // GenSpawn extends MonadCancel, so get:
  // pure, map/flatMap, raiseError, uncancelable, start

  // Cats typeclass Spawn[F] is an alias for GenSpawn[F, E]
  val spawnIO = Spawn[IO] // fetch the given / implicit Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- spawnIO.start(io) // io.start assumes the presence of a Spawn[IO]
    result <- fib.join
  } yield result

  import cats.syntax.flatMap.* // flatMap
  import cats.syntax.functor.* // map

  // generalise
  import cats.effect.syntax.spawn.* // start extension methods
  def effectOnSomeThread[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    // fib <- spawn.start(fa)
    fib <- fa.start
    result <- fib.join
  } yield result

  val molOnFibre = ioOnSomeThread(mol)
  val molOnFibre_v2 = effectOnSomeThread(mol)
  
  // Exercise
  
  // Generalise the following code
  /*
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> IO("A won").debug >> effectA.map(Left.apply)
        case Errored(e) => fibB.cancel >> IO.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => IO("B won").debug >> effectB.map(Right.apply)
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }

      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> IO("B won").debug >> effectB.map(Right.apply)
        case Errored(e) => fibA.cancel >> IO.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => IO("A won").debug >> effectA.map(Left.apply)
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }
    }
   */

  def simpleRace[F[_], A, B](fa: F[A], fb: F[B])(using s: Spawn[F]) : F[Either[A, B]] =
    s.racePair(fa, fb).flatMap {
      case Left((outA, fibB)) => outA match {
        case Succeeded(effectA) => fibB.cancel >> s.pure("A won").debug >> effectA.map(Left.apply)
        case Errored(e) => fibB.cancel >> s.raiseError(e)
        case Canceled() => fibB.join.flatMap {
          case Succeeded(effectB) => s.pure("B won").debug >> effectB.map(Right.apply)
          case Errored(e) => s.raiseError(e)
          case Canceled() => s.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }

      case Right((fibA, outB)) => outB match {
        case Succeeded(effectB) => fibA.cancel >> s.pure("B won").debug >> effectB.map(Right.apply)
        case Errored(e) => fibA.cancel >> s.raiseError(e)
        case Canceled() => fibA.join.flatMap {
          case Succeeded(effectA) => s.pure("A won").debug >> effectA.map(Left.apply)
          case Errored(e) => s.raiseError(e)
          case Canceled() => s.raiseError(new RuntimeException("Both computations cancelled"))
        }
      }
    }
  

  override def run: IO[Unit] =
//    simpleRace(IO.sleep(1.second) >> IO("hello"), IO.sleep(500.millis) >> IO("winner")).debug.void
  simpleRace(IO.sleep(500.millis) >> IO("winner"), IO.sleep(1.second) >> IO("dolly")).debug.void
}
