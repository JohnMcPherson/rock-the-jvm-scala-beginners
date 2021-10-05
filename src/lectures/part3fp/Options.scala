package lectures.part3fp

import java.util.Random


object Options extends App{
  val myFirstOption: Option[Int] = Some(4)
  val noOption: Option[Int] = None
  println(myFirstOption.get)
//  println(noOption.get)
  def unsafeMethod() : String = null
  val result = Option(unsafeMethod())
  println(result)

  val config: Map[String, String] = Map(
    // fetched from elsewhere
    "host" -> "176.45.36.1",
    "port" -> "80"
  )

  class Connection {
    def connect = "Connected"
  }

  object Connection {
    val random = new Random(System.nanoTime())

    def apply (host: String, port: String) : Option[Connection] = {
      if (random.nextBoolean()) Some(new Connection)
      else None
    }
  }

  val host = config.get("host")
  val port = config.get("port")

  val connection = host.flatMap(h => (port.flatMap(p => Connection.apply(h, p))))
  val connectionStatus = connection.map(c => c.connect)
  println("connectionStatus = " + connectionStatus)
  connectionStatus.foreach(println)

  val connectionStatusFor = for {
    h <- host
    p <- port
    c <- Connection.apply(h, p)
  } yield {
    println("Only printed if connected " + c.connect)
    c.connect
  }
  println("connectionStatusFor = " + connectionStatusFor)

}
