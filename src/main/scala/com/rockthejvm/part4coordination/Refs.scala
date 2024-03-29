package com.rockthejvm.part4coordination

import cats.effect.{IO, IOApp, Ref}
import com.rockthejvm.utils.*
import scala.concurrent.duration.*

object Refs extends IOApp.Simple {
  // ref = purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42) // wrapped in an IO as allocating a reference is an effect
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  // modifying is an effect
  val increasedMol: IO[IO[Unit]] = atomicMol.map { ref => // atomicMol is also an effect
    ref.set(43) // this is an effect
  }

  // so we usually flatmap
  val increasedMol_v2: IO[Unit] = atomicMol_v2.flatMap { ref =>
    ref.set(43) // atomic reference means that this op is always thread-safe
  }

  // obtain a value
  val mol: IO[Int] = atomicMol.flatMap(_.get) // also thread-safe

  val getAndSetMol: IO[Int] = atomicMol.flatMap(_.getAndSet(43)) // sets to 43, returns old value

  // updating with a function
  val fMol: IO[Unit] = atomicMol.flatMap(_.update(_ * 10))

  val updatedMol: IO[Int] = atomicMol.flatMap { ref =>
    ref.updateAndGet(_ * 10) // get the new value
    // can also use getAndUpdate to get the old value
  }

  // modifying with a function and returning a different type
  val modifiedMol: IO[String] = atomicMol.flatMap { ref =>
    ref.modify(value => (value * 10, s"my current value is $value"))
  }

  def carFun: IO[Unit] = {
    class Car(make: String)

    val aCar = new Car("ford")

    val aDelayedCar: IO[Car] = IO {
      println("new car built!")
      new Car("ford")
    }

    // same car - 1 car built
    // com.rockthejvm.part4coordination.Refs$Car@1786f9d5
    // com.rockthejvm.part4coordination.Refs$Car@1786f9d5

    // IO(aCar).debug >> IO(aCar).debug

    // different cars, as only built when IO is executed - 2 cars built
    // com.rockthejvm.part4coordination.Refs$Car@61c4c4b3
    // com.rockthejvm.part4coordination.Refs$Car@78297970

    // aDelayedCar.debug >> aDelayedCar.debug

    // same car - 1 car built
    for {
      car <- aDelayedCar
      _ <- IO(car).debug
      _ <- IO(car).debug
    } yield ()

    //    IO.unit
  }

  def atomic10: IO[Unit] = {
    // Similar deal with refs
    val atomic10: IO[Ref[IO, Int]] = IO.ref {
      println("Making 10!") // only one 10 made though.....
      10
    }

    // this doesn't carry updates through - different refs
    // [io-compute-0] 10
    // [io-compute-0] 430
    // [io-compute-0] 10
    //    atomic10.flatMap(ref => IO.print(s"$ref --> ") >> ref.get.debug) >>
    //      atomic10.flatMap { ref => IO.print(s"$ref --> ") >> ref.updateAndGet(_ => 430).debug } >>
    //      atomic10.flatMap(ref => IO.print(s"$ref --> ") >> ref.get.debug).void

    // whereas this does - same ref
    // [io-compute-7] 10
    // [io-compute-7] 430
    // [io-compute-7] 430
    for {
      ref <- atomic10
      _ <- IO.print(s"$ref --> ") >> ref.get.debug
      _ <- IO.print(s"$ref --> ") >> ref.updateAndGet(_ => 430).debug
      _ <- IO.print(s"$ref --> ") >> ref.get.debug
    } yield ()

    //    IO.unit
  }

  import cats.syntax.parallel.*

  def demoConcurrentWorkImpure(l: List[String]): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount = count + wordCount
        _ <- IO(s"New total: $newCount").debug
        _ = (count = newCount)
      } yield ()
    }

    l.map(task)
      .parSequence
      .void
  }

  def demoConcurrentWorkImpure_v2(l: List[String]): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += wordCount)
      } yield ()
    }

    l.map(task)
      .parSequence
      .void
  }

  // Drawbacks
  // - hard to read / debug
  // - mixing pure and impure code
  // - NOT THREAD SAFE

  def demoConcurrentWorkPure(l: List[String]): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      count <- IO.ref(0)
      _ <- l.map(str => task(str, count)).parSequence
    } yield ()
  }

  def demoConcurrentWorkPureBroken(l: List[String]): IO[Unit] = {
    val counter = IO.ref(0)

    def task(workload: String, counter: IO[Ref[IO, Int]]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        total <- counter
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      _ <- l.map(str => task(str, counter)).parSequence
    } yield ()
  }

  // Exercise
  private def tickingClockImpure(): IO[Unit] = {
    var ticks: Long = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- IO(ticks += 1) // not thread safe
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug // not thread safe
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  private def tickingClockPure(): IO[Unit] = {
    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticksRef: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      ticks <- ticksRef.get
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks(ticksRef)
    } yield ()

    for {
      ticks <- IO.ref(0L)
      _ <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ()
  }

  private def tickingClockWeird(): IO[Unit] = {
    val ticks = IO.ref(0L) // IO[Ref] - an effect that gives you a new reference

    def tickingClock: IO[Unit] = for {
      t <- ticks // ticks will give you a NEW ref
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- t.update(_ + 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      t <- ticks // ticks will give you a NEW ref
      _ <- IO.sleep(5.seconds)
      currentTicks <- t.get
      _ <- IO(s"TICKS: $currentTicks").debug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ()
  }

  override def run: IO[Unit] =
    carFun
//    val words = List("I love cats effect", "This ref thing is useless", "I am writing a lot of code")
//    demoConcurrentWorkPureBroken(words)
//    tickingClockImpure()
//    tickingClockWeird()
}
