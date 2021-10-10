package lectures.part4pm

import scala.language.postfixOps

object PatternMatching extends App {
  trait Expr

  case class Number(val n: Int) extends Expr

  case class Sum(e1: Expr, e2: Expr) extends Expr

  case class Prod(e1: Expr, e2: Expr) extends Expr

  def show(expr: Expr): String = expr match {
    case Number(n) => n.toString
    case Sum(e1, e2) => show(e1) + " + " + show(e2)
    case Prod(e1, e2) =>
      def maybeShowParenthesis(internalExp: Expr): String = internalExp match {
        case sum: Sum => "(" + show(sum) + ")"
        case _ => show(internalExp)
      }

      maybeShowParenthesis(e1) + " * " + maybeShowParenthesis(e2)
  }

  println(show(Number(6)))
  println(show(Sum(Number(2), Number(6))))
  println(show(Prod(Number(3), Number(4))))
  println(show(Prod(Sum(Number(3), Number(11)), Number(4))))
  println(show(Prod(Number(4), Sum(Number(3), Number(11)))))
  println(show(Prod(Sum(Number(2), Number(6)), Sum(Number(3), Number(11)))))
  println(show(
    Prod(Number(2),
      Prod(Number(4), Number(6))
    )))
}
