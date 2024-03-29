package com.rockthejvm.part4coordination

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Deferred, IO, IOApp, Ref}
import cats.syntax.parallel.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import scala.util.Random
import scala.collection.immutable.Queue

abstract class Mutex {
  def acquire(id: Int): IO[Unit]

  def release: IO[Unit]
}

def taskId(id: Int) = s"[task $id]"

object MyMutex {
  def create: IO[Mutex] = for {
    waiting <- IO.ref(Queue[Deferred[IO, Unit]]())
  } yield new Mutex:
    override def acquire(id: Int): IO[Unit] =
      for {
        defer <- IO.deferred[Unit]
        updatedClients <- waiting.updateAndGet { _ :+ defer }
        _ <- if (updatedClients.length == 1) {
          IO(s"${taskId(id)} Mutex acquired").debug >> defer.complete(())
        } else {
          IO(s"${taskId(id)} Mutex queued - blocking").debug >> defer.get
        }
      } yield ()

    override def release: IO[Unit] = for {
      updatedClients <- waiting.updateAndGet { c => c.drop(1) }
      _ <- updatedClients.headOption match
        case Some(defer) => defer.complete(())
        case None => IO.unit
    } yield ()
}

type Signal = Deferred[IO, Unit]

case class State(locked: Boolean, waiting: Queue[Signal])

val unlocked = State(false, Queue())

def createSignal(): IO[Signal] = IO.deferred[Unit]

object MyTextbookMutex {
  def create: IO[Mutex] = IO.ref(unlocked).map { stateRef =>
    new Mutex:
      // if mutex is currently unlocked, state becomes (true, [])
      // if mutex is locked, state becomes (true, queue + new signal) and wait on new signal
      override def acquire(id: Int): IO[Unit] =
        for {
          defer <- IO.deferred[Unit]
          oldState <- stateRef.getAndUpdate { state =>
            if (state.locked) {
              state.copy(waiting = state.waiting :+ defer)
            } else {
              state.copy(locked = true)
            }
          }
          _ <- if (oldState.locked) {
            IO(s"${taskId(id)} Mutex queued - blocking").debug >> defer.get
          } else {
            IO(s"${taskId(id)} Mutex acquired").debug >> defer.complete(())
          }
        } yield ()

      // if mutex is unlocked, leave state unchanged
      // if mutex is locked
      //   - if queue empty, unlock the mutex
      //   - if queue not empty, take first signal out and complete it
      override def release: IO[Unit] = for {
        maybeSignal <- stateRef.modify { state =>
          if (!state.locked) {
            (state, Option.empty[Signal])
          }
          else {
            state.waiting.headOption match
              case Some(signal) => (state.copy(waiting = state.waiting.drop(1)), Option(signal))
              case None => (state.copy(locked = false), Option.empty[Signal])
          }
        }
        _ <- maybeSignal match {
          case Some(signal) => signal.complete(())
          case None => IO.unit
        }
      } yield ()
  }
}

object Mutex {
  def create: IO[Mutex] = IO.ref(unlocked).map { state =>
    new Mutex:
      // if mutex is currently unlocked, state becomes (true, [])
      // if mutex is locked, state becomes (true, queue + new signal) and wait on new signal
      override def acquire(id: Int): IO[Unit] = createSignal().flatMap { signal =>
        state.modify {
          case State(false, _) => (
            State(locked = true, Queue()),
            IO(s"${taskId(id)} Mutex acquired").debug >> IO.unit
          )
          case State(true, queue) => (
            State(locked = true, queue.enqueue(signal)),
            IO(s"${taskId(id)} Mutex queued - blocking").debug >> signal.get
          )
        }.flatten
      }

      // if mutex is unlocked, leave state unchanged
      // if mutex is locked
      //   - if queue empty, unlock the mutex
      //   - if queue not empty, take first signal out and complete it
      override def release: IO[Unit] =
        state.modify {
          case State(false, _) => (unlocked, IO.unit)
          case State(true, queue) => queue.headOption match
            case Some(signal) => (
              State(locked = true, waiting = queue.dequeue._2),
              signal.complete(()).void
            )
            case None => (unlocked, IO.unit)
        }.flatten
  }
}

object MutexWithCancellation {

  def create: IO[Mutex] = IO.ref(unlocked).map(create)

  private def create(state: Ref[IO, State]): Mutex =
    new Mutex:
      override def acquire(id: Int): IO[Unit] = IO.uncancelable { poll =>
        createSignal().flatMap { signal =>

          // remove the signal that the cancelled fiber was waiting on
          val cleanup = state.modify {
            case State(locked, queue) => (
              State(locked, queue.filterNot(_ eq signal)),
              // release the mutex
              IO(s"${taskId(id)} cancellation cleanup").debug >> release
            )
          }.flatten

          state.modify {
            case State(false, _) => (
              State(locked = true, Queue()),
              IO(s"${taskId(id)} Mutex acquired").debug >> IO.unit
            )
            case State(true, queue) => (
              State(locked = true, queue.enqueue(signal)),
              // blocking on signal should be cancelable
              IO(s"${taskId(id)} Mutex queued - blocking").debug >> poll(signal.get).onCancel(cleanup)
            )
          }.flatten
        }
      }

      // should not be cancellable
      override def release: IO[Unit] =
        // state.modify is an atomic operation - so it's already cancellation aware
        state.modify {
          case State(false, _) => (unlocked, IO.unit)
          case State(true, queue) => queue.headOption match
            case Some(signal) => (State(locked = true, waiting = queue.dequeue._2), signal.complete(()).void)
            case None => (unlocked, IO.unit)
        }.flatten
}


object MutexPlayground extends IOApp.Simple {

  private def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  private def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"${taskId(id)} working...").debug
    res <- criticalTask()
    _ <- IO(s"${taskId(id)} got result: $res").debug
  } yield res

  private def demoNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  private def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"${taskId(id)} waiting for permission...").debug
    _ <- mutex.acquire(id) // blocks if mutex has been acquired by some other fiber
    // critical section - start
    _ <- IO(s"${taskId(id)} working...").debug
    res <- criticalTask()
    _ <- IO(s"${taskId(id)} got result: $res").debug
    // critical section - start
    _ <- mutex.release
    _ <- IO(s"${taskId(id)} lock removed.").debug
  } yield res

  def demoLockingTasks(): IO[List[Int]] = for {
    mutex <- MutexWithCancellation.create
    result <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
  } yield result

  private def createCancellingTask(id: Int, mutex: Mutex): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else for {
      fib <- createLockingTask(id, mutex).onCancel(IO(s"${taskId(id)} received canacellation!").debug.void).start
      _ <- IO.sleep(2.seconds) >> fib.cancel
      out <- fib.join
      result <- out match {
        case Succeeded(effect) => effect
        case Errored(_) => IO(-1)
        case Canceled() => IO(-2)
      }
    } yield result
  }

  private def demoCancellingTasksWithNormalMutex(): IO[List[Int]] = for {
    mutex <- Mutex.create
    result <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
  } yield result

  private def demoCancellingTasksWithMutexThatSupportsCancellation(): IO[List[Int]] = for {
    mutex <- MutexWithCancellation.create
    result <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
  } yield result

  override def run: IO[Unit] =
    demoCancellingTasksWithMutexThatSupportsCancellation().debug.void
  // demoCancellingTasksWithNormalMutex().debug.void
  // demoLockingTasks().debug.void
  // demoNonLockingTasks().debug.void
}
