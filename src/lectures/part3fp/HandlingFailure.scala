package lectures.part3fp

import scala.util.{Random, Try}

object HandlingFailure extends App {
  val host = "localhost"
  val port = "8080"

  val random = new Random(System.nanoTime())
  val randomBool1: Boolean = random.nextBoolean()
  val randomBool2: Boolean = random.nextBoolean()

  def renderHTML(page: String): Unit = println(page)

  class Connection {
    def get(url: String): String = {
      if (randomBool2) "<html>...</html>"
      else throw new RuntimeException("Connection interrupted")
    }

    def getSafe (url: String): Try[String] = Try(get(url))
  }

  object HttpService {

    def getConnection(host: String, port: String): Connection =
      if (randomBool1) new Connection
      else throw new RuntimeException("Someone else took the port")

    def getSafeConnection(host: String, port: String): Try[Connection] =
      Try(getConnection(host, port))
  }

  val connection: Try[Connection] = Try(HttpService.getConnection(host, port))
  if (connection.isSuccess) {
    println("Got a connection")
    val page: Try[String] = Try(connection.get.get("zzz"))
    if (page.isSuccess) {
      println("Got a page")
      println(page.get)
    }
    else println(page)
  }
  else println(connection)

  def getPossibleSafeConnection(host: String, port: String): Try[Connection] =
    Try(HttpService.getConnection(host, port))

  def getPossibleSafeHtml(possibleSafeConnection: Try[Connection], url: String): Try[String] =
    possibleSafeConnection.flatMap(c => Try(c.get(url)))

  val possibleConnection = getPossibleSafeConnection(host, port)
  val possibleSafeHtmlFromPossibleConnection = getPossibleSafeHtml(possibleConnection, "junkURL")
  possibleSafeHtmlFromPossibleConnection.foreach(html => println("HTML is: " + html))

  HttpService.getSafeConnection(host, port)
    .flatMap(c => c.getSafe("junk"))
    .foreach(html => println("Html using safe calls in classes is: " + html))

  getPossibleSafeConnection(host, port)
    .flatMap(c => getPossibleSafeHtml(Try(c), "jun"))
    .foreach(html => println("Html using safe calls outside classes is: " + html))

  for {
    connection1 <- getPossibleSafeConnection(host, port)
    html <- getPossibleSafeHtml(Try(connection1), "Junk")
  } println("Html using for: " + html)
}
