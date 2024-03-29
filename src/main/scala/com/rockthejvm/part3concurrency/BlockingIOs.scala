package com.rockthejvm.part3concurrency

import com.rockthejvm.utils.*
import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

// Example output (all run on same thread):
// [io-compute-4] An IO
// [io-compute-4] A second IO
// [io-compute-4] A third IO
val someIOs = for {
  _ <- IO("An IO").debug
  _ <- IO("A second IO").debug
  _ <- IO("A third IO").debug
} yield ()

// These are run on different threads:
// [io-compute-0] ()
// [io-compute-5] ()
// [io-compute-3] ()
val someSleeps = for {
  // semantic blocking - the threads that the CE manages are not blocked i.e. no thread sleeps
  // for 1 second. Instead a thread is scheduled to continue the calculation after 1 second.
  // This explains whyb the debug statements output different threads.
  // Put another way, this effect looks like it's blocked for 1 second, but no actual thread
  // is blocked for that amount of time. In ordr to implement, effects have to be able to
  // yield control od the threads they are running on.
  _ <- IO.sleep(1.second).debug
  _ <- IO.sleep(1.second).debug
  _ <- IO.sleep(1.second).debug
} yield ()

// really blocking IOs:
// [io-blocking-0] computed a blocking code
// This code is evaluated on a thread from ANOTHER thread pool specific for blocking calls
val aBlockingIO = IO.blocking {
  Thread.sleep(1000)
  println(s"[${Thread.currentThread().getName}] computed a blocking code")
  42
}

// yielding - doesn't work due to CE optimisations
val iosOnManyThreads = for {
  _ <- IO("first").debug
  // a signal to yield control over the thread - a hint to CE runtime to start running the rest of the for
  // comprehension on a different thread - equivalent to IO.shift in CE2
  _ <- IO.cede
  _ <- IO("second").debug // the rest of this effect may run on another thread (not necessarily)
  _ <- IO.cede
  _ <- IO("third").debug
} yield ()

// we can get thread switching to occur....
// not yet - the CE thread pool is really smart about how to batch computations onto the same thread to increase performance
val thousandCedes = (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug)

// if we can get CE to run this computation on a thread pool that the CE runtime has less control over
// then the thread switching might occur

// Now get different threads e.g.
// [pool-1-thread-6] 862
// [pool-1-thread-4] 969
// [pool-1-thread-5] 970

// At the end: CE is telling us about a thread pool that it doesn't control. It's our job to shut it down.
// Non-daemon threads currently preventing JVM termination: - 33: Thread[pool-1-thread-8,5,main]
// -  - 30: Thread[pool-1-thread-5,5,main]
// -  - 32: Thread[pool-1-thread-7,5,main]
// -  - 27: Thread[pool-1-thread-2,5,main]
// -  - 25: Thread[pool-1-thread-1,5,main]
// -  - 35: Thread[DestroyJavaVM,5,main]
// -  - 31: Thread[pool-1-thread-6,5,main]
// -  - 28: Thread[pool-1-thread-3,5,main]
// -  - 29: Thread[pool-1-thread-4,5,main]
// val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
// val thousandCedesOnEC = thousandCedes.evalOn(ec)

// Put it all into a method, and shut it down properly
def testThousandEffectsSwitch(): IO[Int] = {
  val service = Executors.newFixedThreadPool(8)
  val ec: ExecutionContext = ExecutionContext.fromExecutorService(service)
  for {
    result <- (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
    _ <- IO(service.shutdown())
  } yield result
}

// IO blocking calls & IO.sleep implement semantic blocking - they yield control over the calling thread automatically

object BlockingIOs extends IOApp.Simple {
  override def run: IO[Unit] = testThousandEffectsSwitch().void
  // thousandCedesOnEC.void
  // thousandCedes.void
  // iosOnManyThreads
  // aBlockingIO.void
  // someSleeps
  // someIOs
}
