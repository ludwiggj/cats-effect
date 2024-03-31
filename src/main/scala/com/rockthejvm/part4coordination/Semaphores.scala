package com.rockthejvm.part4coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import cats.syntax.parallel.*
import com.rockthejvm.utils.*
import scala.concurrent.duration.*
import scala.util.Random

object Semaphores extends IOApp.Simple {

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // example - limiting number of concurrent sessions on a server
  private def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(3.second) >> IO(Random.nextInt(100))

  private def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquire
    // critical section
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.release
  } yield res

  private def demoSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1fib <- login(1, sem).start
    user2fib <- login(2, sem).start
    user3fib <- login(3, sem).start
    _ <- user1fib.join
    _ <- user2fib.join
    _ <- user3fib.join
  } yield ()

  private def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session $id] waiting to log in...").debug
    _ <- sem.acquireN(requiredPermits) // all or nothing - must acquire all of them at the same time
    // critical section
    _ <- IO(s"[session $id] logged in, working...").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"[session $id] done: $res, logging out...").debug
    // end of critical section
    _ <- sem.releaseN(requiredPermits)
  } yield res

  private def demoWeightedSemaphore() = for {
    sem <- Semaphore[IO](2)
    user1fib <- weightedLogin(1, 1, sem).start
    user2fib <- weightedLogin(2, 2, sem).start
    user3fib <- weightedLogin(3, 3, sem).start // this will never start
    _ <- user1fib.join
    _ <- user2fib.join
    _ <- user3fib.join
  } yield ()

  // Exercise
  // Semaphore with 1 permit == mutex
  private val mutex = Semaphore[IO](1)
  // All threads execute in parallel - oops!
  val users: IO[List[Int]] = (1 to 10).toList.parTraverse { id =>
    for {
      sem <- mutex // creates a new semaphore per thread!
      _ <- IO(s"[session $id] waiting to log in...").debug
      _ <- sem.acquire
      // critical section
      _ <- IO(s"[session $id] logged in, working...").debug
      res <- doWorkWhileLoggedIn()
      _ <- IO(s"[session $id] done: $res, logging out...").debug
      // end of critical section
      _ <- sem.release
    } yield res
  }

  private val usersFixed: IO[List[Int]] = for {
    sem <- mutex
    results <- (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").debug
        _ <- sem.acquire
        // critical section
        _ <- IO(s"[session $id] logged in, working...").debug
        result <- doWorkWhileLoggedIn()
        _ <- IO(s"[session $id] done: $result, logging out...").debug
        // end of critical section
        _ <- sem.release
      } yield result
    }
  } yield results

  private val usersFixedBetter: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session $id] waiting to log in...").debug
        _ <- sem.acquire
        // critical section
        _ <- IO(s"[session $id] logged in, working...").debug
        result <- doWorkWhileLoggedIn()
        _ <- IO(s"[session $id] done: $result, logging out...").debug
        // end of critical section
        _ <- sem.release
      } yield result
    }
  }

  override def run: IO[Unit] =
    usersFixedBetter.debug.void
    // usersFixed.debug.void
    // users.debug.void
    // demoWeightedSemaphore()
    // demoSemaphore()
}
