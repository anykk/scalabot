object Main {
  def main(args: Array[String]): Unit = {
    val parser = CommandParser
    val manager = PollManager

    while (true){
      val command = scala.io.StdIn.readLine("Command: ")
      println(manager.applyCommand(manager.polls, parser.parse(command)))
    }
  }
}
