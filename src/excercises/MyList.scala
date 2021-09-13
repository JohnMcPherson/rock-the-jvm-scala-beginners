package excercises

import scala.annotation.tailrec

abstract case class MyList[+A]() {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B] = new Cons(element, this)

  def elements: String

  def printElements() : Unit = println(elements)

  def reverse: MyList[A]

  def map[B](transformer: MyTransformer[A, B]) : MyList[B]

  def flatmap[B](transformer: MyTransformer[A, MyList[B]]) : MyList[B]

  def filter (predicate : MyPredicate[A]) : MyList[A]

  def ++[B >: A] (list : MyList[B]) : MyList[B]

  override def toString: String = "[" + elements + "]"
}

object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def elements = ""

  override def reverse: EmptyList.type = this

  override def map[B] (transformer: MyTransformer[Nothing, B]) : MyList[B] = EmptyList

  override def filter(predicate: MyPredicate[Nothing]): MyList[Nothing] = EmptyList

  override def flatmap[B](transformer: MyTransformer[Nothing, MyList[B]]): MyList[B] = EmptyList

  override def ++[B] (list : MyList[B]) : MyList[B] = list.reverse //works - but maybe for the wrong reason
}

class Cons[+A](val head: A, val tail: MyList[A]) extends MyList[A] {
  def isEmpty: Boolean = false

  def elements: String = head + (if (tail.isEmpty) "" else ", " + tail.elements)

  override def reverse: MyList[A] = {
    @tailrec
    def reverseHelper(partialResult: MyList[A] = EmptyList, remainingTail: MyList[A]): MyList[A] = {
      val nextResult: MyList[A] = partialResult.add(remainingTail.head)
      val nextTail = remainingTail.tail
      if (nextTail.isEmpty)
        nextResult
      else
        reverseHelper(nextResult, nextTail)
    }

    reverseHelper(remainingTail = this)
  }

  override def map[B](transformer : MyTransformer[A, B]) : MyList[B] = {
    @tailrec
    def mapHelper(partialResult : MyList[B] = EmptyList, remainingInputTail: MyList[A]) : MyList[B] = {
        val nextPartialResult = partialResult.add(transformer.transform(remainingInputTail.head))
        val tailOfRemainingInput = remainingInputTail.tail
      if (tailOfRemainingInput.isEmpty) nextPartialResult
        else
          mapHelper(nextPartialResult,tailOfRemainingInput)
    }
    mapHelper(remainingInputTail = this)
      .reverse

    // new Cons[C](transformer.transform(head), tail.map(transformer)) //answer from course - but not tail recursive. Could Stack Overflow
  }

  override def filter (predicate: MyPredicate[A]): MyList[A] = {
    @tailrec
    def filterHelper(partialResult : MyList[A] = EmptyList, remainingInputTail: MyList[A]) : MyList[A] = {
      val nextPartialResult = {
        if (predicate.test(remainingInputTail.head)) partialResult.add(remainingInputTail.head)
        else partialResult
      }
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else
        filterHelper(nextPartialResult,nextRemainingInputTail)
    }
    filterHelper(remainingInputTail = this).reverse
  }

  override def flatmap[C](transformer: MyTransformer[A, MyList[C]]): MyList[C] = {

    @tailrec
    def flatmapHelper(partialResult : MyList[C] = EmptyList, remainingInputTail: MyList[A]) : MyList[C] = {
      val nextPartialResult = {
        partialResult ++ (transformer.transform(remainingInputTail.head))
      }
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else
        flatmapHelper(nextPartialResult,nextRemainingInputTail)
    }
    flatmapHelper(remainingInputTail = this).reverse
  }

  override def ++[C >: A](list: MyList[C]): MyList[C] = {
    addListToList(this,list)

  }

  def addListToList[C](firstList: MyList[C], secondList: MyList[C]): MyList[C] = {
    @tailrec
    def helperForAddToList(partialResult: MyList[C], remainingInputTail: MyList[C]): MyList[C] = {
      val nextPartialResult = partialResult.add(remainingInputTail.head)
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else helperForAddToList(nextPartialResult, nextRemainingInputTail)
    }
    helperForAddToList(firstList, secondList)
  }
}



trait MyPredicate[-T] {
  def  test(input: T) : Boolean
}

class EvenPredicate extends MyPredicate[Int] {
   override def test(input : Int) : Boolean = {
    (input % 2  == 0)
  }
}

trait MyTransformer[-A, B] {
  def transform(input : A) : B
}

class StringToIntTransformer extends MyTransformer[String, Int] {
  override def transform(input: String): Int = input.length
}

class IntToListOf2IntsTransformer extends MyTransformer[Int, MyList[Int]] {
  override def transform(input: Int): MyList[Int] = EmptyList.add(input * 2).add(input)
}

object ListTester extends App {
  val startTime = System.currentTimeMillis()
  val emptyList = EmptyList
  val populatedList = emptyList.add(1).add(2).add(3)
  val listOfIntsAndStrings = populatedList.add("four")
  val reversedList = listOfIntsAndStrings.reverse
  val listOf1 = emptyList.add(1)
  println(System.currentTimeMillis() - startTime)
  emptyList.printElements()
  emptyList.reverse.printElements()
  println(listOfIntsAndStrings.tail.isEmpty)
  listOfIntsAndStrings.printElements()
  reversedList.printElements()
  println(emptyList.isEmpty)
  println(listOf1.tail.isEmpty)
  listOf1.printElements()
  listOf1.reverse.printElements()
  println(new EvenPredicate().test(4))
  println(new StringToIntTransformer().transform("length"))
  val populatedListOfStrings = EmptyList.add("teste6").add("test5").add("tes4")
  println(populatedListOfStrings.toString)
  val transformedStringsToInts = populatedListOfStrings.map(new StringToIntTransformer)
  transformedStringsToInts.printElements()
  val evenTransformedToInts = transformedStringsToInts.filter(new EvenPredicate)
  evenTransformedToInts.printElements()
  val intsTransformedToListOfListOfInts = transformedStringsToInts.map(new IntToListOf2IntsTransformer)
  intsTransformedToListOfListOfInts.printElements()
  val flattenedListOfInts = transformedStringsToInts.flatmap(new IntToListOf2IntsTransformer)
  flattenedListOfInts.printElements()
  val clonedPopulatedList = emptyList.add(1).add(2).add(3)
  println("Clone is equal to original = " + (clonedPopulatedList == populatedList))
 }

