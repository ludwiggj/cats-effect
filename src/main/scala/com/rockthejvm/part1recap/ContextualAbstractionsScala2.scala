package com.rockthejvm.part1recap

object ContextualAbstractionsScala2 {
  // implicit classes
  case class Person(name: String) {
    def greet(): String = s"Hi my name is $name"
  }

  implicit class PersonableString(name: String) {
    def greet(): String = Person(name).greet()
  }

  // standard library example
  import scala.concurrent.duration.*
  val aSecond = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit inc: Int): Int = x + inc

  def multiply(x: Int)(implicit factor: Int): Int = x * factor

  // more complex example
  trait JSONSerializer[T] {
    def toJsonValue(value: T): String
  }

  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String =
    serializer.toJsonValue(value)

  implicit val personSerializer: JSONSerializer[Person] =
    (value: Person) => s"""{"name": "${value.name}"}"""

  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] =
    (values: List[T]) => s"[${values.map(serializer.toJsonValue).mkString(",")}]"

  def main(args: Array[String]): Unit = {
    // extension method
    println("Peter".greet())

    println(aSecond)

    implicit val theInc: Int = 10

    println(increment(2)) // 12
    println(multiply(10)) // 100

    println(convertToJson(Person("guy")))

    println(convertToJson(List(Person("guy"), Person("Glenys"))))
  }
}
