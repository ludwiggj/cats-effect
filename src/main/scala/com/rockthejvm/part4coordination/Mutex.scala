package com.rockthejvm.part4coordination

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Concurrent, Deferred, IO, IOApp, Ref}
import cats.syntax.parallel.*
import com.rockthejvm.part4coordination.MutexUtils.State
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import scala.util.Random
import scala.collection.immutable.Queue

abstract class Mutex[F[_]] {
  def acquire(id: Int): F[Unit]

  def release: F[Unit]
}

object MutexUtils {
  def taskId(id: Int) = s"[task $id]"

  type Signal = Deferred[IO, Unit]

  case class State(locked: Boolean, waiting: Queue[Signal])

  val unlocked = State(false, Queue())

  def createSignal(): IO[Signal] = IO.deferred[Unit]
}

object MyMutex {
  def create: IO[Mutex[IO]] = for {
    waiting <- IO.ref(Queue[Deferred[IO, Unit]]())
  } yield new Mutex:
    override def acquire(id: Int): IO[Unit] =
      for {
        defer <- IO.deferred[Unit]
        updatedClients <- waiting.updateAndGet {
          _ :+ defer
        }
        _ <- if (updatedClients.length == 1) {
          IO(s"${MutexUtils.taskId(id)} Mutex acquired").debug >> defer.complete(())
        } else {
          IO(s"${MutexUtils.taskId(id)} Mutex queued - blocking").debug >> defer.get
        }
      } yield ()

    override def release: IO[Unit] = for {
      updatedClients <- waiting.updateAndGet { c => c.drop(1) }
      _ <- updatedClients.headOption match
        case Some(defer) => defer.complete(())
        case None => IO.unit
    } yield ()
}

object MyTextbookMutex {
  def create: IO[Mutex[IO]] = IO.ref(MutexUtils.unlocked).map { stateRef =>
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
            IO(s"${MutexUtils.taskId(id)} Mutex queued - blocking").debug >> defer.get
          } else {
            IO(s"${MutexUtils.taskId(id)} Mutex acquired").debug >> defer.complete(())
          }
        } yield ()

      // if mutex is unlocked, leave state unchanged
      // if mutex is locked
      //   - if queue empty, unlock the mutex
      //   - if queue not empty, take first signal out and complete it
      override def release: IO[Unit] = for {
        maybeSignal <- stateRef.modify { state =>
          if (!state.locked) {
            (state, Option.empty[MutexUtils.Signal])
          }
          else {
            state.waiting.headOption match
              case Some(signal) => (state.copy(waiting = state.waiting.drop(1)), Option(signal))
              case None => (state.copy(locked = false), Option.empty[MutexUtils.Signal])
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
  def create: IO[Mutex[IO]] = IO.ref(MutexUtils.unlocked).map { state =>
    new Mutex:
      // if mutex is currently unlocked, state becomes (true, [])
      // if mutex is locked, state becomes (true, queue + new signal) and wait on new signal
      override def acquire(id: Int): IO[Unit] = MutexUtils.createSignal().flatMap { signal =>
        state.modify {
          case State(false, _) => (
            State(locked = true, Queue()),
            IO(s"${MutexUtils.taskId(id)} Mutex acquired").debug >> IO.unit
          )
          case State(true, queue) => (
            State(locked = true, queue.enqueue(signal)),
            IO(s"${MutexUtils.taskId(id)} Mutex queued - blocking").debug >> signal.get
          )
        }.flatten
      }

      // if mutex is unlocked, leave state unchanged
      // if mutex is locked
      //   - if queue empty, unlock the mutex
      //   - if queue not empty, take first signal out and complete it
      override def release: IO[Unit] =
        state.modify {
          case State(false, _) => (MutexUtils.unlocked, IO.unit)
          case State(true, queue) => queue.headOption match
            case Some(signal) => (
              State(locked = true, waiting = queue.dequeue._2),
              signal.complete(()).void
            )
            case None => (MutexUtils.unlocked, IO.unit)
        }.flatten
  }
}

object MutexWithCancellation {
  def create: IO[Mutex[IO]] = IO.ref(MutexUtils.unlocked).map(create)

  private def create(state: Ref[IO, State]): Mutex[IO] =
    new Mutex:
      override def acquire(id: Int): IO[Unit] = IO.uncancelable { poll =>
        MutexUtils.createSignal().flatMap { signal =>

          // remove the signal that the cancelled fiber was waiting on
          // otherwise when we complete it, that fiber won't release the mutex (since it was cancelled)
          val cleanup = state.modify {
            case State(locked, queue) => (
              State(locked, queue.filterNot(_ eq signal)),
              // release the mutex
              IO(s"${MutexUtils.taskId(id)} cancellation cleanup").debug >> release
            )
          }.flatten

          state.modify {
            case State(false, _) => (
              State(locked = true, Queue()),
              IO(s"${MutexUtils.taskId(id)} Mutex acquired").debug >> IO.unit
            )
            case State(true, queue) => (
              State(locked = true, queue.enqueue(signal)),
              // blocking on signal should be cancelable
              IO(s"${MutexUtils.taskId(id)} Mutex queued - blocking").debug >> poll(signal.get).onCancel(cleanup)
            )
          }.flatten
        }
      }

      // should not be cancellable
      override def release: IO[Unit] =
        // state.modify is an atomic operation - so it's already cancellation aware
        state.modify {
          case State(false, _) => (MutexUtils.unlocked, IO.unit)
          case State(true, queue) => queue.headOption match
            case Some(signal) => (State(locked = true, waiting = queue.dequeue._2), signal.complete(()).void)
            case None => (MutexUtils.unlocked, IO.unit)
        }.flatten
}

object GeneralisedMutexWithCancellation {
  import cats.syntax.functor.*
  import cats.syntax.flatMap.*
  import cats.effect.syntax.monadCancel.*
  import com.rockthejvm.utils.general.*

  type Signal[F[_]] = Deferred[F, Unit]

  case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

  def unlocked[F[_]] = State[F](false, Queue())

  def createSignal[F[_]](using concurrent: Concurrent[F]): F[Signal[F]] = concurrent.deferred[Unit]

  def create[F[_]](using concurrent: Concurrent[F]): F[Mutex[F]] =
    concurrent.ref(unlocked).map(create(_))

  private def create[F[_]](using concurrent: Concurrent[F])(state: Ref[F, State[F]]): Mutex[F] =
    new Mutex:
      override def acquire(id: Int): F[Unit] = concurrent.uncancelable { poll =>
        createSignal.flatMap { signal =>

          // remove the signal that the cancelled fiber was waiting on
          // otherwise when we complete it, that fiber won't release the mutex (since it was cancelled)
          val cleanup = state.modify {
            case State(locked, queue) =>
              (
                State(locked, queue.filterNot(_ eq signal)),
                // release the mutex
                concurrent.pure(s"${MutexUtils.taskId(id)} cancellation cleanup").debug >> release
              )
          }.flatten

          state.modify {
            case State(false, _) => (
              State(locked = true, Queue()),
              concurrent.pure(s"${MutexUtils.taskId(id)} Mutex acquired").debug >> concurrent.unit
            )
            case State(true, queue) => (
              State(locked = true, queue.enqueue(signal)),
              // blocking on signal should be cancelable
              concurrent.pure(s"${MutexUtils.taskId(id)} Mutex queued - blocking").debug >> poll(signal.get).onCancel(cleanup)
            )
          }.flatten
        }
      }

      // should not be cancellable
      override def release: F[Unit] =
        // state.modify is an atomic operation - so it's already cancellation aware
        state.modify {
          case State(false, _) => (unlocked, concurrent.unit)
          case State(true, queue) => queue.headOption match
            case Some(signal) => (State(locked = true, waiting = queue.dequeue._2), signal.complete(()).void)
            case None => (unlocked, concurrent.unit)
        }.flatten
}


object MutexPlayground extends IOApp.Simple {

  private def criticalTask(): IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  private def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"${MutexUtils.taskId(id)} working...").debug
    res <- criticalTask()
    _ <- IO(s"${MutexUtils.taskId(id)} got result: $res").debug
  } yield res

  private def demoNonLockingTasks(): IO[List[Int]] = (1 to 10).toList.parTraverse(id => createNonLockingTask(id))

  private def createLockingTask(id: Int, mutex: Mutex[IO]): IO[Int] = for {
    _ <- IO(s"${MutexUtils.taskId(id)} waiting for permission...").debug
    _ <- mutex.acquire(id) // blocks if mutex has been acquired by some other fiber
    // critical section - start
    _ <- IO(s"${MutexUtils.taskId(id)} working...").debug
    res <- criticalTask()
    _ <- IO(s"${MutexUtils.taskId(id)} got result: $res").debug
    // critical section - start
    _ <- mutex.release
    _ <- IO(s"${MutexUtils.taskId(id)} lock removed.").debug
  } yield res

  def demoLockingTasks(): IO[List[Int]] = for {
    mutex <- MutexWithCancellation.create
    result <- (1 to 10).toList.parTraverse(id => createLockingTask(id, mutex))
  } yield result

  private def createCancellingTask(id: Int, mutex: Mutex[IO]): IO[Int] = {
    if (id % 2 == 0) createLockingTask(id, mutex)
    else for {
      fib <- createLockingTask(id, mutex).onCancel(IO(s"${MutexUtils.taskId(id)} received canacellation!").debug.void).start
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
    mutex <- GeneralisedMutexWithCancellation.create[IO]
    result <- (1 to 10).toList.parTraverse(id => createCancellingTask(id, mutex))
  } yield result

  override def run: IO[Unit] =
    demoCancellingTasksWithMutexThatSupportsCancellation().debug.void
  // demoCancellingTasksWithNormalMutex().debug.void
  // demoLockingTasks().debug.void
  // demoNonLockingTasks().debug.void
}
