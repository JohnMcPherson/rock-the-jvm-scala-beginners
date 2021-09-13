package excercises

object OOBasicsExcercise extends App {
  val theAuthor = new Writer("Jim", "Beam", 1959)
  println(theAuthor.fullName)
  val novel = new Novel("My first novel", 2006, theAuthor)
  println(novel.toString())
  val revisedNovel = novel.copy(2011)
  println(revisedNovel.toString())
  println(novel.isWrittenBy(theAuthor))
  println(revisedNovel.isWrittenBy(theAuthor))
  val imposter = new Writer("Jim", "Beam", 1959)
  println(novel.isWrittenBy(imposter))
  val counter = (new Counter)
  counter.print //0
  val firstIncrement = counter.increment
  firstIncrement.print //1
  val secondIncrement = firstIncrement.increment(7)
  secondIncrement.print // 8
  val thirdIncrement = secondIncrement.decrement(2)
  thirdIncrement.print // 6
  val fourthIncrement = thirdIncrement.decrement
  fourthIncrement.print //5
}

class Writer (firstName : String, surname : String, val yearOfBirth: Int) {
  def fullName() : String = s"$firstName $surname"
}

class Novel (val name: String, val yearOfRelease: Int, val author : Writer) {
  def authorAge : Int = yearOfRelease - author.yearOfBirth
  def isWrittenBy(author : Writer) : Boolean = author == this.author
  def copy(newYearOfRelease : Int) : Novel = new Novel(name, newYearOfRelease, author)
  override def toString : String = s"$name $author $authorAge"
}

class Counter (val count : Int = 0) {
  def increment : Counter = {
    println("incrementing")
    new Counter(count + 1)
  }
  def decrement : Counter = {
    println("decrementing")
    new Counter(count + 1)
  }
  def increment(incValue: Int) : Counter = {
    if (incValue <= 0) this
    else increment.increment(incValue-1)
  }
  def decrement(decValue: Int) : Counter = {
    if (decValue <= 0) this
    else decrement.decrement(decValue-1)
  }

  def print: Unit = println(count)
}
