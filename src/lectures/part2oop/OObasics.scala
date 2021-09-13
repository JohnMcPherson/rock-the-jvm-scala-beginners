package lectures.part2oop

object OObasics extends App{
  val person = new Person ("John", 26)
  println(person.x)
  person.greet("Daniel")
  person.greet()
}

class Person (name : String, val age : Int) {
  val x = 2
  println(1 + 3)

  def greet(name: String): Unit = println(s"${this.name} says: Hi, $name")
  def greet():Unit = println(s"Hi, I am $name")
}
