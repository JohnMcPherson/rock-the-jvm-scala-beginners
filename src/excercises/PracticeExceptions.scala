package excercises

class PocketCalculator {
  def add(x : Int, y : Int): Int = {
      val retval = x + y
      if (x > 0  && y > 0 && retval < 0) throw new OverflowException
      retval
  }
  def subtract(x : Int, y : Int): Int = x - y
  def multiply(x : Int, y : Int): Int = x * y
  def divide(x : Int, y : Int): Int = x / y
}

class OverflowException extends Exception

class UnderflowException extends Exception


object PracticeExceptions extends App{


//  val largeArray = Array.ofDim(Int.MaxValue)
  def crashStackOverflow() : Int = 1 + crashStackOverflow()
  //val crash = crashStackOverflow()
  val calculator = new PocketCalculator
  println(calculator.add(3,4))
  println(calculator.add(Int.MaxValue, 5))
}
