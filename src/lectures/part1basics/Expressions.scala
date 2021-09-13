package lectures.part1basics

object Expressions extends App {

  var aVariable = 2
  println(aVariable)
  val wierdExpression = aVariable + 3
  println(wierdExpression)
  println(aVariable)
  val codeBlock = {
    if (aVariable == 3) "True" else 0
  }
  println (codeBlock)
  println(raw"a \n b")
  println("c \n d")
  val escaped = "e \n f"
  println(raw"$escaped")
  println(escaped)
}
