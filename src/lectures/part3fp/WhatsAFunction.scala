package lectures.part3fp

import scala.annotation.tailrec

object WhatsAFunction extends App {
  def superAdder: (Int) => ((Int) => Int) = { (x: Int) =>
    (y: Int) =>
      x + y
  }

  @tailrec
  def nTimes(f : Int => Int, n : Int, x : Int) : Int =
    if (n <= 0) x
    else nTimes(f, n -1, f(x))

  val add1 = (x : Int) => x + 1
  val multiplyBy10 : (Int => Int) = (x : Int)  => x * 10

  println(nTimes(add1, 10, 1))
  println(nTimes(multiplyBy10, 9, 1))

  val adder3 = superAdder(3)
  println(adder3(4))
  println(superAdder(3)(4)) //curried

  def nTimesBetter(f: Int => Int, n: Int) : (Int => Int) =
    if (n <= 0)  (x: Int) => x
    else (x: Int) => nTimesBetter(f, n-1)(f(x))

  println (nTimesBetter(add1,10)(1))
  println (nTimesBetter(multiplyBy10,1)
    (2))
  val plus10 = nTimesBetter(add1, 10)
  println(plus10(1))
}
