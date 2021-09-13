package excercises

import scala.annotation.tailrec

object Functions extends App{

  def greeting (name: String, age : Int) : String = {
    "Hi I am " + name + " and I am " + age + " years old"
  }
  println(greeting("John", 62))

  def factorial (n: Long) : Long = {
    if (n <= 1) 1 else
      n * factorial(n -1)
  }

  println(factorial(11))

  def concatonateStringNTimes(n : Int, string: String) : String = {
    @tailrec
    def concatHelper(m: Int, intermediateString : String): String = {
      if (m <= 0) intermediateString
      else {
        val newString = intermediateString + string
        concatHelper(m - 1, newString)
      }
    }
    concatHelper(n, "")
  }
  println(concatonateStringNTimes(4, "myString "))

  def fibonacci (n: Long) : Long = {
    if (n < 3) 1 else
      fibonacci(n - 1) + fibonacci(n-2)
  }

  def fib(n : BigInt) : BigInt = {
    @tailrec
    def fibonacciTailRec(m: BigInt = 3, fibMminus1: BigInt = 1, fibMminus2: BigInt = 1): BigInt = {
      if (n <= 2) 1
      else {
        val fibM = fibMminus2 + fibMminus1
        if (m >= n) fibM
        else {
          fibonacciTailRec(m + 1, fibM, fibMminus1)
        }
      }
    }
    fibonacciTailRec()
  }

  println(fib(1))
  println(fib(2))
  println(fib(3))
  println(fib(4))
  println(fib(5))
  println(fib(6))
  println(fib(7))
  println(fib(5000))

  def isPrime(n: Long): Boolean = {

    @tailrec
    def isDivisibleByThisorGreater(divisor: Long): Boolean = {
      if (divisor * divisor > n) false else
        (n % divisor == 0) || isDivisibleByThisorGreater(divisor + 1)
    }

    !isDivisibleByThisorGreater(2)
  }

//  printPrimes(20000000)

  def printPrimes(n : Long) : Unit = {
    @tailrec
    def printPrimesFromStartToN(start : Long) : Unit = {
      if (start > n) return
      if (isPrime(start)) println(start)
      printPrimesFromStartToN(start + 1)
    }
    printPrimesFromStartToN(1)
  }

  def infinite() : Int = infinite() + 1

  def printXonly(x: Int, y: => Int) : Unit = println(x)

  printXonly(691, infinite())
}
