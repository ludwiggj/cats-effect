package com.rockthejvm.part2effects

import scala.concurrent.Future
import scala.io.StdIn

object L01_Effects {
  // Pure functional programming
  // Expression that produces a value without side effects
  // Substitution

  def combine(a: Int, b: Int): Int = a + b

  // Substitution
  val five: Int = combine(2, 3)
  val five_v2: Int = 2 + 3
  val five_v3: Int = 5

  // referential transparency = can replace an expression with its value as many times as we want without changing behaviour

  // example: print to the console, breaks referential transparency (impure)
  val printSomething: Unit = println("Cats Effect")
  val printSomething_v2: Unit = () // not the same

  // example: changing a variable's value (impure)
  var anInt = 0
  val changingVar: Unit = anInt += 1
  val changingVar_2: Unit = () // not the same

  // side effects are inevitable for useful programs

  // effect - desirable properties
  // (1) Type signature describes kind of calculation that will be preformed
  // (2) Type signature describes the value that will be calculated
  // (3) When side effects are needed, effect construction is separate from effect execution

  // example - option - an effect type

  // (1) Calculation that may return value or may not
  // (2) Computes value of wrapped type, if it exists e.g. Int
  // (3) Doesn't perform side effects - although it could be made to, see below
  val anOption: Option[Int] = Option(42)

  // example - future - not an effect type

  // (1) Describes asynchronous computation
  // (2) Computes value of type A, if it's successful
  // (3) Side effect is required (allocating / scheduling a thread)
  //     Execution is not separate from construction (evaluated eagerly)

  import scala.concurrent.ExecutionContext.Implicits.global

  val aFuture: Future[Int] = Future(41)

  // example - MyIO datatype - an effect type

  // (1) Describes any computation that might produce side effects
  // (2) Calculates a value of type A, if it's successful
  // (3) Side effects are required for evaluation of () => A
  //     Creation of MyIO does not produce side effects on construction
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("Yo")
    42
  })

  def combineIO(a: Int, b: Int): MyIO[Int] = MyIO(() => combine(a, b))

  // Substitution
  val willBeFive: MyIO[Int] = combineIO(2, 3)
  val willBeFive_v2: MyIO[Int] = MyIO(() => combine(2, 3))
  val willBeFive_v3: MyIO[Int] = MyIO(() => 5) // from previous simplification

  // So it's still a program that will produce the number 5

  // Exercises

  // (1) An IO which returns current time of system
  def clock: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // (2) An IO which measured duration of a computation
  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime <- clock
    _         <- computation // throw away result
    endTime   <- clock
  } yield endTime - startTime

  // clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))

  // (1) Last term
  //     clock.map(finishTime => finishTime - startTime)
  //  => MyIO(() => clock.unsafeRun() - startTime)
  //  => MyIO(() => System.currentTimeMillis() - startTime)

  // clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))

  // (2) Next last term

  // computation.flatMap(lambda) = MyIO(() => lambda(computation).unsafeRun())

  // In this case, lambda is _ => MyIO(() => System.currentTimeMillis() - startTime)

  //                             = MyIO(() => MyIO(() => System.currentTimeMillis() - startTime).unsafeRun())

  //                             = MyIO(() => System.currentTimeMillis()_after_computation() - startTime)

  // (3) Next last time i.e. first term

  // clock.flatMap(startTime => MyIO(() => System.currentTimeMillis()_after_computation() - startTime))

  // = MyIO(() => lambda(clock.unsafeRun()).unsafeRun())

  // = MyIO(() => lambda(System.currentTimeMillis()).unsafeRun())

  // = MyIO(() => MyIO(() => System.currentTimeMillis()_after_computation() - System.currentTimeMillis_before_computation()).unsafeRun())

  // = MyIO(() => System.currentTimeMillis()_after_computation() - System.currentTimeMillis_before_computation()))

  // (3) An IO which prints something to the console
  def printIt(msg: String): MyIO[Unit] = MyIO(() => println(msg))

  // (4) An IO which reads a line (a string) from the std input
  def readIt(): MyIO[String] = MyIO(() => StdIn.readLine())

  def main(args: Array[String]): Unit = {
    Option[Int]({
      println("hi"); 2
    })
    anIO.unsafeRun()

    println(clock.unsafeRun())
    println(measure(anIO).unsafeRun())

    def sleepyIO: MyIO[Unit] = MyIO(() => Thread.sleep(1000))

    println(measure(sleepyIO).unsafeRun())

    printIt("Hello!").unsafeRun()

    readIt().unsafeRun()

    val program: MyIO[Unit] = for {
      r1 <- readIt()
      r2 <- readIt()
      _ <- printIt(r1 + r2)
    } yield ()

    // Expansion

    // MyIO(() => StdIn.readLine()).flatMap(
    //     r1 => MyIO(() => StdIn.readLine()).flatMap(
    //       r2 => MyIO(() => println(r1 + r2)).map(
    //         _ => ()
    // )))

    // case class MyIO[A](unsafeRun: () => A) {
    //   def map[B](f: A => B): MyIO[B] =
    //     MyIO(() => f(unsafeRun()))
    // etc...

    // (1) Map

    // MyIO(() => println(r1 + r2)).map(_ => ())

    // => MyIO(() => { println(r1 + r2) => () })

    // Now have:

    // MyIO(() => StdIn.readLine()).flatMap(
    //     r1 => MyIO(() => StdIn.readLine()).flatMap(
    //       r2 => MyIO(() => { println(r1 + r2) => () })
    // ))

    // (2) flatMap

    //  MyIO(() => StdIn.readLine()).flatMap(r2 => MyIO(() => { println(r1 + r2) => () }))
    //                                       <------------- lambda --------------------->

    //  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    //    MyIO(() => f(unsafeRun()).unsafeRun())

    // Introducing lambda

    //  MyIO(() => StdIn.readLine()).flatMap(lambda))

    // Substituting flatMap

    //  MyIO(() => lambda(StdIn.readLine()).unsafeRun())

    // Substituting lambda

    //  MyIO(() => MyIO(() => { println(r1 + StdIn.readLine()) => () }).unsafeRun())

    //  MyIO(() => { println(r1 + StdIn.readLine()) => () })

    // Now have:

    // MyIO(() => StdIn.readLine()).flatMap(
    //     r1 => MyIO(() => { println(r1 + StdIn.readLine()) => () })
    //     <----------------------- lambda ------------------------->
    // )

    // Just repeat the same substitution - introduce lambda

    // MyIO(() => StdIn.readLine()).flatMap(lambda)

    // Substituting flatMap

    //  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    //    MyIO(() => f(unsafeRun()).unsafeRun())

    // MyIO(() => lambda(StdIn.readLine()).unsafeRun())

    // Substituting lambda

    // MyIO(() => MyIO(() => { println(StdIn.readLine() + StdIn.readLine()) => () }).unsafeRun())

    // MyIO(() => { println(StdIn.readLine() + StdIn.readLine()) => () })
    
    program.unsafeRun()
  }
}
