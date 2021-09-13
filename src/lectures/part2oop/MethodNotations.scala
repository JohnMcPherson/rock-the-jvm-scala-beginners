package lectures.part2oop

import scala.language.postfixOps

object MethodNotations extends App {
  class Person (val name: String, val favouriteMovie: String, val age : Int = 30) {
    def likes(movie: String): Boolean = movie == favouriteMovie

    def +(person: Person): String = s"${this.name} is hanging out with ${person.name}"

    def +(nickname: String): Person = new Person (s"${this.name} ($nickname)", favouriteMovie, age)

    def unary_+ : Person = new Person (name,favouriteMovie, age + 1)

    def learns (topic : String) : String = s"$name learns $topic"
    def learnsScala = this learns "Scala"
    def apply() :String = s"Hi, my name is $name and my favourite movie is $favouriteMovie"

    def apply(numberTimes : Int) = s"$name watched $favouriteMovie $numberTimes times"
  }

  val mary = new Person ("Mary", "Inception", 32)
  println(mary.likes("Inception"))
  println(mary likes "Inception")
  val tom = new Person("Tom", "Fight Club")
  println(mary + tom)
  println((mary)())
  println((mary + "the Rocker")(4))

  println(mary.age)
  val agedMary = +mary
  println(agedMary.age)
  println(mary learns "Agile")
  println(mary learnsScala)
  println (agedMary(4))


  println (1 + 2)
  println(1.+ (2))
}
