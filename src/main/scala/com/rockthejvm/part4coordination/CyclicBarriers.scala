package com.rockthejvm.part4coordination

import cats.effect.std.CyclicBarrier
import cats.effect.{Deferred, IO, IOApp}
import cats.syntax.parallel.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import scala.util.Random

object CyclicBarriers extends IOApp.Simple {
  // A cyclic barrier is a coordination primitive that
  // - is initialised with a count
  // - has a single API: await

  // A cyclic barrier will semantically block all fibers calling its await method until we have
  // exactly N fibers waiting. At which point the barrier will unblock all fibers and reset to
  // its original state.
  // Any further fiber will again block until we have exactly N fibers waiting.
  // ...
  // And so on...

  // example - signing up for a social media site that's just about to be launched

//  def createUser(id: Int, barrier: CyclicBarrier[IO]): IO[Unit] = for {
  def createUser(id: Int, barrier: CBarrier): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble * 500).toInt.millis)
    _ <- IO(s"[user $id] Just heard there's a new social network - signing up for the wait list!").debug
    _ <- IO.sleep((Random.nextDouble * 1500).toInt.millis)
    _ <- IO(s"[user $id] Now on the wait list!").debug
    _ <- barrier.await // block the fiber until there are exactly N users waiting
    _ <- IO(s"[user $id] OMG this is so cool!").debug
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("[announcer] The Rock the JM social network is open for registration. Launching when we have 10 users!").debug
//    barrier <- CyclicBarrier[IO](10)
    barrier <- CBarrier(10)
    _ <- (1 to 20).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  // Create our own cyclic barrier - ignore cancellation
  abstract class CBarrier {
    def await: IO[Unit]
  }

  object CBarrier {
    case class State(waitingCount: Int, signal: Deferred[IO, Unit])

    def apply(threshold: Int): IO[CBarrier] = for {
      signal <- Deferred[IO, Unit]
      state <- IO.ref(State(threshold, signal))
    } yield new CBarrier:
      override def await: IO[Unit] = Deferred[IO, Unit].flatMap { newSignal =>
        state.modify {
          case State(1, signal) =>
            (State(threshold, newSignal), signal.complete(()).void)

          case State(waitingCount, signal) =>
            (State(waitingCount - 1, signal), signal.get)
        }.flatten
      }
  }

  override def run: IO[Unit] = openNetwork()
}
