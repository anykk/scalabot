object Main {
  val parser = CommandParser

  def main(args: Array[String]): Unit = {
    loop()
  }


  @scala.annotation.tailrec
  def loop(): Unit = {
    val input = scala.io.StdIn.readLine("Comandu mraz:")

    if (input == "/bye") return

    println(parser.parse(input))
    loop()
  }
}
