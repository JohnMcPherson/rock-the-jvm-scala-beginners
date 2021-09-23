package lectures.part3fp

import excercises.{EmptyList, MyList}

object MapFlatmapFilterFor extends App {
  val list = List(1,2,3)
  println(list)
  println(list.map(_ + 1))
  println(list.map(_ + 1 + " is a number"))

  val toPair : (Int) => List[Int] = (x: Int) => List(x, x + 1)
  println(list.map(toPair).flatten)
  println(list.flatMap(toPair))

  val numbers = List(1,2,3,4)
  val chars = List('a', 'b', 'c', 'd')

  val combine : (List[Int], List[Char]) => List[String] =
    (numList, charList) => charList.flatMap(c => numList.map(n => "" + c + n))
  val combinedNumbersToString = combine(numbers, chars).toString()
  println(combinedNumbersToString)
  assert(combinedNumbersToString.equals("List(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4)"))

  val myListNumbers = EmptyList.add(4).add(3).add(2).add(1)
  val myListChars = EmptyList.add('d').add('c').add('b').add('a')

  val myListCombine : (MyList[Int], MyList[Char]) => MyList[String] =
    (numList, charList) => charList.flatMap(c => numList.filter(_ % 2 == 0).map(n => "" + c + n))
  val myListCombinedNumbersToString = myListCombine(myListNumbers, myListChars).toString()
  println(myListCombinedNumbersToString)
  assert(myListCombinedNumbersToString.equals("[a2, a4, b2, b4, c2, c4, d2, d4]"))

  val myListCombinedUsingForComp = for {
    c <- myListChars
    n <- myListNumbers if n % 2 == 0
  } yield "" + c + n

  println(myListCombinedUsingForComp.toString)
  assert(myListCombinedNumbersToString.equals(myListCombinedUsingForComp.toString))

}
