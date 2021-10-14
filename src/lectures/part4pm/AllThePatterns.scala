package lectures.part4pm

object AllThePatterns extends App{
  val numbers : List[Int] = List(1, 2, 3)
  val numbersMatch = numbers match {
    case myList: List[Any] if (myList.head == myList.head.toString) => "strings"
    case listOfNumbers: List[Int] => "a list of numbers"
  }

  println(numbersMatch)
}
