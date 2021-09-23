package lectures.part3fp

sealed abstract class Maybe[+T] {
  def map[B](transformer: T => B): Maybe[B]

  def flatMap[B](transformer: T => Maybe[B]): Maybe[B]

  def filter(predicate: T => Boolean): Maybe[T]

  def value: Any
}

object MaybeNot extends Maybe[Nothing] {
  override def map[B](transformer: Nothing => B): Maybe[B] = MaybeNot

  override def flatMap[B](transformer: Nothing => Maybe[B]): Maybe[B] = MaybeNot

  override def filter(predicate: Nothing => Boolean): Maybe[Nothing] = MaybeNot

  override def value: Any = MaybeNot

  override def toString = "NoResult"
}

class Just[+T](val result: T) extends Maybe[T] {
  override def map[B](transformer: T => B): Maybe[B] = new Just(transformer(result))

  override def flatMap[B](transformer: T => Maybe[B]): Maybe[B] = transformer(result)

  override def filter(predicate: T => Boolean): Maybe[T] =
    if (predicate(result)) this
    else MaybeNot

  override def value: Any = this.result

  override def toString: String = value.toString
}

object MaybeTest extends App {
  val resultName = new Just("name")
  val nameLength = resultName.map(it => it.length)
  println(nameLength)
  assert(nameLength.value.equals(4))

  val nameLengthFM = resultName.flatMap(it => new Just(it.length))
  println(nameLengthFM)
  assert(nameLengthFM.value.equals(4))

  val result3 = new Just(3)
  val isEven = (x : Int) => x % 2 == 0

  println(nameLength.filter(isEven))
  assert(nameLength.filter(isEven).value.equals(4))

  println(result3.filter(isEven))
  assert(result3.filter(isEven).value.equals(MaybeNot))

}
