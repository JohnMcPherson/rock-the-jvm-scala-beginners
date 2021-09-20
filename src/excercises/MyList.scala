package excercises

import excercises.EmptyList.addListToList

import scala.annotation.tailrec

abstract case class MyList[+A]() {
  def head: A

  def tail: MyList[A]

  def isEmpty: Boolean

  def add[B >: A](element: B): MyList[B] = new Cons(element, this)

  def elements: String

  def printElements() : Unit = println(elements)

  def reverse: MyList[A]

  def map[B](transformer: A => B) : MyList[B]

  def flatmap[B](transformer: A => MyList[B]) : MyList[B]

  def filter (predicate : A => Boolean) : MyList[A]

  def ++[B >: A] (list : MyList[B]) : MyList[B]

  def addListToList[C](firstList: MyList[C], secondList: MyList[C]): MyList[C] = {
    @tailrec
    def helperForAddToList(partialResult: MyList[C], remainingInputTail: MyList[C]): MyList[C] = {
      val nextPartialResult = partialResult.add(remainingInputTail.head)
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else helperForAddToList(nextPartialResult, nextRemainingInputTail)
    }
    helperForAddToList(secondList, firstList.reverse)
  }

  def forEach (f: A => Unit) : MyList[A]

  def sort (compare: (A, A) => Int) : MyList[A]

  def zipWith[C >: A, B] (other : MyList[C], f: (A, C) => B) : MyList[B]

  override def toString: String = "[" + elements + "]"
}

object EmptyList extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException

  def tail: MyList[Nothing] = throw new NoSuchElementException

  def isEmpty: Boolean = true

  def elements = ""

  override def reverse: EmptyList.type = this

  override def map[B] (transformer: (Nothing) => B) : MyList[B] = EmptyList

  override def filter(predicate: (Nothing) => Boolean): MyList[Nothing] = EmptyList

  override def flatmap[B](transformer: Nothing => MyList[B]): MyList[B] = EmptyList

  override def ++[B] (list : MyList[B]) : MyList[B] = list

  override def forEach(f: Nothing => Unit): MyList[Nothing] = EmptyList

  override def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = EmptyList

  override def zipWith[C >: Nothing, B](other: MyList[C], f: (Nothing, C) => B): MyList[B] = EmptyList
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

  override def map[B](transformer : (A) => B) : MyList[B] = {
    @tailrec
    def mapHelper(partialResult : MyList[B] = EmptyList, remainingInputTail: MyList[A]) : MyList[B] = {
        val nextPartialResult = partialResult.add(transformer(remainingInputTail.head))
        val tailOfRemainingInput = remainingInputTail.tail
      if (tailOfRemainingInput.isEmpty) nextPartialResult
        else
          mapHelper(nextPartialResult,tailOfRemainingInput)
    }
    mapHelper(remainingInputTail = this)
      .reverse

    // new Cons[C](transformer.transform(head), tail.map(transformer)) //answer from course - but not tail recursive. Could Stack Overflow
  }

  override def filter (predicate: (A) => Boolean): MyList[A] = {
    @tailrec
    def filterHelper(partialResult : MyList[A] = EmptyList, remainingInputTail: MyList[A]) : MyList[A] = {
      val nextPartialResult = {
        if (predicate(remainingInputTail.head)) partialResult.add(remainingInputTail.head)
        else partialResult
      }
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else
        filterHelper(nextPartialResult,nextRemainingInputTail)
    }
    filterHelper(remainingInputTail = this).reverse
  }

  override def flatmap[C](transformer: A => MyList[C]): MyList[C] = {

    @tailrec
    def flatmapHelper(partialResult : MyList[C] = EmptyList, remainingInputTail: MyList[A]) : MyList[C] = {
      val nextPartialResult = {
        val transformedHeadOfRemainingInputTail = transformer(remainingInputTail.head)
        partialResult ++ (transformedHeadOfRemainingInputTail)
      }
      val nextRemainingInputTail = remainingInputTail.tail
      if (nextRemainingInputTail.isEmpty) nextPartialResult
      else
        flatmapHelper(nextPartialResult,nextRemainingInputTail)
    }
    flatmapHelper(remainingInputTail = this)
  }

  override def ++[C >: A](list: MyList[C]): MyList[C] = {
    addListToList(this,list)

  }

  override def forEach(f: A => Unit): MyList[A] = {
    @tailrec
    def forEachHelper(remainingTail : MyList[A] = this): Unit = {
      if (remainingTail.isEmpty) this
      else {
        f(remainingTail.head)
        forEachHelper(remainingTail.tail)
      }
    }
    forEachHelper()
    this
  }

  override def sort(compare: (A, A) => Int): MyList[A] = {
    @tailrec
    def insert(value: A, reverseSortedHeadList: MyList[A] = EmptyList, sortedTail: MyList[A]): MyList[A] = {
      def isBetweenHeadOfLists: Boolean = {
        def valGreaterOrEqualHeadOfReversedList(): Boolean = {
          (reverseSortedHeadList.isEmpty || compare(value, reverseSortedHeadList.head) >= 0)
        }

        def valLessOrEqualHeadOfSortedList(): Boolean = {
          sortedTail.isEmpty || compare(value, sortedTail.head) <= 0
        }

        valGreaterOrEqualHeadOfReversedList() && valLessOrEqualHeadOfSortedList()
      }

      if (isBetweenHeadOfLists) reverseSortedHeadList.add(value).reverse ++ sortedTail
      else insert(value, reverseSortedHeadList.add(sortedTail.head), sortedTail.tail)
  }

    @tailrec
    def sortHelper(remainingUnsorted: MyList[A] = this, sortedTail: MyList[A] = EmptyList): MyList[A] = {
      if (remainingUnsorted.isEmpty) sortedTail
      else sortHelper(remainingUnsorted.tail, insert(remainingUnsorted.head, reverseSortedHeadList = EmptyList, sortedTail = sortedTail))
    }

    sortHelper()
  }

  override def zipWith[C >: A, B](other: MyList[C], f: (A, C) => B): MyList[B] = {
    @tailrec
    def zipWithHelper(partialResult: MyList[B] = EmptyList, remainingThisTail: MyList[A] = this, remainingOtherTail: MyList[C] = other) : MyList[B] = {
      if (remainingThisTail.isEmpty) partialResult
      else {
        val newPartialResult = partialResult.add(f(remainingThisTail.head, remainingOtherTail.head))
        zipWithHelper(newPartialResult, remainingThisTail.tail, remainingOtherTail.tail)
      }
    }
    zipWithHelper().reverse
  }
}

object ListTester extends App {
  val isEven : (Int) => Boolean = (input: Int) => input % 2 == 0
  val stringToInt : (String) => Int = (input : String) => input.length
  val intToListOf2Ints : (Int) => MyList[Int] = (input: Int) => EmptyList.add(input * 2).add(input)
  val emptyList = EmptyList
  val populatedList = emptyList.add(1).add(2).add(3)
  val listOfIntsAndStrings = populatedList.add("four")
  val reversedList = listOfIntsAndStrings.reverse
  val listOf1 = emptyList.add(1)
  println("Empty list: " + emptyList)
  assert(emptyList.toString.equals("[]"))
  println("Empty list reversed: " + emptyList.reverse)
  assert(emptyList.reverse.toString.equals("[]"))
  println("listOfIntsAndStrings.tail.isEmpty (false): " + listOfIntsAndStrings.tail.isEmpty)
  assert(!listOfIntsAndStrings.isEmpty)
  println("listOfIntsAndStrings ([four, 3, 2, 1]) : " + listOfIntsAndStrings)
  assert(listOfIntsAndStrings.toString.equals("[four, 3, 2, 1]"))
  println("reversedList ([1, 2, 3, four]) : " + reversedList)
  assert(reversedList.toString.equals("[1, 2, 3, four]"))
  assert(emptyList.isEmpty)
  assert(listOf1.tail.isEmpty)
  listOf1.printElements()
  assert(listOf1.toString.equals("[1]"))
  listOf1.reverse.printElements()
  assert(listOf1.toString.equals("[1]"))
  println(isEven(4))
  assert(isEven(4))
  println(stringToInt("length"))
  assert(stringToInt("length").equals(6))
  val populatedListOfStrings = EmptyList.add("teste6").add("test5").add("tes4")
  println(populatedListOfStrings.toString)
  assert(populatedListOfStrings.toString.equals("[tes4, test5, teste6]"))
  val transformedStringsToInts = populatedListOfStrings.map(stringToInt)
  transformedStringsToInts.printElements()
  assert (transformedStringsToInts.toString.equals("[4, 5, 6]"))
  val evenTransformedToInts = transformedStringsToInts.filter(_ % 2 == 0)
  evenTransformedToInts.printElements()
  assert (evenTransformedToInts.toString.equals("[4, 6]"))
  println("Odds using anonymous function = " + transformedStringsToInts.filter((input: Int) => !isEven(input)))
  assert (transformedStringsToInts.filter((input: Int) => !isEven(input)).toString.equals("[5]"))
  val intsTransformedToListOfListOfInts = transformedStringsToInts.map(intToListOf2Ints)
  intsTransformedToListOfListOfInts.printElements()
  assert(intsTransformedToListOfListOfInts.toString.equals("[[4, 8], [5, 10], [6, 12]]"))
  val firstList = EmptyList.add(1).add(2).add(3).reverse
  println(firstList.toString)
  assert(firstList.toString.equals("[1, 2, 3]"))
  val secondList = EmptyList.add(4).add(5).add(6).reverse
  println(secondList.toString)
  assert(secondList.toString.equals("[4, 5, 6]"))
  val combinedListUsingAddListToList = addListToList(firstList, secondList)
  println(combinedListUsingAddListToList)
  assert(combinedListUsingAddListToList.toString.equals("[1, 2, 3, 4, 5, 6]"))
  val combinedListUsingPlusPlusOperator = firstList ++ secondList
  println(combinedListUsingPlusPlusOperator)
  assert(combinedListUsingPlusPlusOperator.toString.equals("[1, 2, 3, 4, 5, 6]"))
  val transformIntToTwoInts = (input: Int) => EmptyList.add(input * 2).add(input)
  val transformThree = transformIntToTwoInts(3)
  println(transformThree)
  assert(transformThree.toString.equals("[3, 6]"))
  val transformThreeAddedToEmptyList = EmptyList ++ transformThree
  println("EmptyList ++ transformThree: " + transformThreeAddedToEmptyList)
  assert(transformThreeAddedToEmptyList.toString.equals("[3, 6]"))
  val flatmappedListOfSingleIntInput = EmptyList.add(4).flatmap(transformIntToTwoInts)
  flatmappedListOfSingleIntInput.printElements()
  assert(flatmappedListOfSingleIntInput.toString.equals("[4, 8]"))
  val flattenedListOfIntsToInts = transformedStringsToInts.flatmap((input: Int) => EmptyList.add(input * 2).add(input))
  flattenedListOfIntsToInts.printElements()
  assert(flattenedListOfIntsToInts.toString.equals("[4, 8, 5, 10, 6, 12]"))
  val clonedPopulatedList = emptyList.add(1).add(2).add(3)
  println("Clone is equal to original = " + (clonedPopulatedList == populatedList))
  assert(clonedPopulatedList == populatedList)
  flattenedListOfIntsToInts.forEach(println)
  val sortedFlattenedListOfInts = flattenedListOfIntsToInts.sort((x: Int, y:Int) => x-y)
  println("Sorted list: " + sortedFlattenedListOfInts)
  assert(sortedFlattenedListOfInts.toString().equals("[4, 5, 6, 8, 10, 12]"))
  val evenTransformedToIntsMappedToOnePlus = evenTransformedToInts.map((x: Int) => x+1)
  println(evenTransformedToIntsMappedToOnePlus)
  assert(evenTransformedToIntsMappedToOnePlus.toString.equals("[5, 7]"))
  val zippedValues = evenTransformedToInts.zipWith(evenTransformedToIntsMappedToOnePlus, (x : Int, y: Int) => x * y)
  println(zippedValues)
  assert(zippedValues.toString.equals("[20, 42]"))
  val addedLists = evenTransformedToInts ++ evenTransformedToIntsMappedToOnePlus
  println(addedLists)
  assert(addedLists.toString.equals("[4, 6, 5, 7]"))
 }

