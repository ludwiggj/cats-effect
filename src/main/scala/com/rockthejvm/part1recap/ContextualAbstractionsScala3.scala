package com.rockthejvm.part1recap

object ContextualAbstractionsScala3 {
  // given/using combo
  def increment(x: Int)(using inc: Int): Int = x + inc

  def multiply(x: Int)(using factor: Int): Int = x * factor

  // more complex example
  trait Combiner[A] {
    def combine(x: A, y:A): A
    def empty: A
  }

  def combineAll[A](values: List[A])(using combiner: Combiner[A]) =
    values.foldLeft(combiner.empty)(combiner.combine)

  object CombinerSyntax {
    given intAdder: Combiner[Int] with {
      def combine(x: Int, y: Int) = x + y

      def empty = 0
    }

    // synthesize given instances
    given optionCombiner[T](using combiner: Combiner[T]): Combiner[Option[T]] with {
      def combine(x: Option[T], y: Option[T]): Option[T] = (x, y) match {
        case (Some(x), Some(y)) => Option(combiner.combine(x, y))
        case _ => Option.empty[T]
      }

      def empty: Option[T] = Option(combiner.empty)
    }
  }

  // extension methods
  case class Person(name: String) {
    def greet(): String = s"Hi my name is $name"
  }

  extension (name: String) {
    def greet(): String = Person(name).greet()
  }

  // generic extension
  extension [T](list: List[T]) {
    def reduceAll(using combiner: Combiner[T]): T =
      list.foldLeft(combiner.empty)(combiner.combine)
  }

  def main(args: Array[String]): Unit = {
    given defaultAmount: Int = 10

    println(increment(2)) // 12
    println(multiply(10)) // 100

    import CombinerSyntax.intAdder
    println(combineAll(List(1, 2, 3, 4)))

    import CombinerSyntax.optionCombiner
    println(combineAll(List(Option(1), Option(2), Option(3))))
    println(combineAll(List(Option(1), None, Option(3))))

    println("Bobby".greet())

    println(List(1, 2, 3, 4).reduceAll)
  }
}
