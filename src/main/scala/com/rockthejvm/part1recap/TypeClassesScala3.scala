package com.rockthejvm.part1recap

object TypeClassesScala3 {
  // Typeclasses specify an interface that attaches behaviour to some types that you want to support the behaviour,
  // but not others

  // (1) Typeclass definition - the behaviour we want to attach
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // (2) The typeclass instances - specifies which types support the interface specified in part (1)
  given stringSerializer: JSONSerializer[String] = (s: String) => s"\"$s\""

  given intSerializer: JSONSerializer[Int] with {
    override def toJson(i: Int): String = i.toString
  }

  case class Person(name: String, age: Int)

  import JSONSyntax.*

  given personSerializer: JSONSerializer[Person] with {
    override def toJson(person: Person): String =
      s"""{"name": ${person.name.toJson}, "age": ${person.age.toJson}}"""
  }

  // (2.1) Utility methods
  given listSerializer[T](using serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    (values: List[T]) => values.map(serializer.toJson).mkString("[", ",", "]")

  // (3) Offer the API
  def serializeToJson[T](value: T)(using serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def serializeListToJson[T](list: List[T])(using serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  // (4) Add extension method
  object JSONSyntax {
    extension[T](value: T) {
      def toJson(using serializer: JSONSerializer[T]): String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    val mrG = Person("Mr Graeme", 52)
    val mrsH = Person("Mrs Audrey", 56)

    val peeps = List(mrG, mrsH)

    println(serializeToJson(mrG))
    println(serializeListToJson(peeps))

    println(mrG.toJson)
    println(peeps.toJson)
  }
}
