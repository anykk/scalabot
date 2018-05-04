import scala.util.{Success, Failure}

object Main {
  def main(args: Array[String]): Unit = {
    //TelegramBot.run()
    val parser = CommandParser
    println(parser.parse(scala.io.StdIn.readLine("Cmd: ")) match {
      case Success(command) => command.toString
      case Failure(exception) => exception.getMessage
    })
  }
}
