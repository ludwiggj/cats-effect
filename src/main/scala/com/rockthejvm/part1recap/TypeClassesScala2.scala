package com.rockthejvm.part1recap

object TypeClassesScala2 {
  // Typeclasses specify an interface that attaches behaviour to some types that you want to support the behaviour,
  // but not others

  // (1) Typeclass definition - the behaviour we want to attach
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // (2) The typeclass instances - specifies which types support the interface specified in part (1)
  //     val flavour
  implicit val stringSerializer: JSONSerializer[String] = (s: String) => s"\"$s\""

  implicit val intSerializer: JSONSerializer[Int] = (i: Int) => i.toString

  //     object flavour
  case class Person(name: String, age: Int)

  import JSONSyntax.*

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(person: Person): String =
      s"""{"name": ${person.name.toJson}, "age": ${person.age.toJson}}"""
  }

  // (2.1) Utility methods
  implicit def listSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    (values: List[T]) => values.map(serializer.toJson).mkString("[", ",", "]")

  // (3) Offer the API
  def serializeToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJson(value)

  def serializeListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  // (4) Add extension method
  object JSONSyntax {
    implicit class JsonSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  def main(args: Array[String]): Unit = {
    val mrG = Person("Graeme", 52)
    val mrsH = Person("Audrey", 56)

    val peeps = List(mrG, mrsH)

    println(JsonSerializable(mrG).toJson)

    println(serializeToJson(mrG))
    println(serializeListToJson(peeps))

    println(mrG.toJson)
    println(peeps.toJson)
  }
}
