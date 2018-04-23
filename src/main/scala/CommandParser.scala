import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.Try
import scala.util.parsing.combinator._

import Requests._


trait CommandParser {
  def parse(string: String): Request
}

object CommandParser extends RegexParsers with CommandParser {
  private val anonymityMap = Map("yes" -> true, "no" -> false)
  private val visibilityMap = Map("afterstop" -> false, "continuous" -> true)

  def leftBracket: Parser[String] = "((" ^^ (_ => "(")

  def rightBracket: Parser[String] = "))" ^^ (_ => ")")

  def string: Parser[String] = """[_a-zA-Zа-яА-ЯёЁ0-9.,:;'"*&!?]+""".r

  def digits: Parser[Int] = """\d+""".r ^^ (_.toInt)

  def date: Parser[Option[LocalDateTime]] =
    """(\d{2}:){2}\d{2} (\d{2}:){2}\d{2}""".r ^^ {
      date => Try(LocalDateTime.parse(date, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))).toOption
    }

  def anonymity: Parser[Boolean] = "(" ~> "yes|no".r <~ ")" ^^ (anonymityMap(_))

  def visibility: Parser[Boolean] = "(" ~> "afterstop|continuous".r <~ ")" ^^ (visibilityMap(_))

  def stringArgument: Parser[String] =
    "(" ~> rep(leftBracket | rightBracket | string) <~ ")" ^^ (xs => xs.mkString(sep=" "))

  def digitArgument: Parser[Int] = "(" ~> digits <~ ")"

  def dateArgument: Parser[Option[LocalDateTime]] = "(" ~> date <~ ")"

  def createPoll: Parser[CreatePollRequest] =
    "/create_poll" ~> stringArgument ~
      opt(
        anonymity ~
          opt(visibility ~
            opt(dateArgument ~
              opt(dateArgument)
            )
          )
      ) ^^ {
      case name ~ None =>
        CreatePollRequest(name)
      case name ~ Some(anonymity ~ None) =>
        CreatePollRequest(name, anonymity)
      case name ~ Some(anonymity ~ Some(visibility ~ None)) =>
        CreatePollRequest(name, anonymity, visibility)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ None))) =>
        CreatePollRequest(name, anonymity, visibility, startTime)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ Some(endTime)))) =>
        CreatePollRequest(name, anonymity, visibility, startTime, endTime)
    }

  def list: Parser[Request] = "/list".r ^^ { _ => ListRequest() }

  def commandWithId = (commandName: String) =>
    commandName.r ~> digitArgument

  def deletePoll: Parser[DeletePollRequest] = commandWithId("/delete_poll") ^^ { id: Int => DeletePollRequest(id) }

  def startPoll: Parser[StartPollRequest] = commandWithId("/start_poll") ^^ { id: Int => StartPollRequest(id) }

  def stopPoll: Parser[StopPollRequest] = commandWithId("/stop_poll") ^^ { id: Int => StopPollRequest(id) }

  def result: Parser[ResultRequest] = commandWithId("/result") ^^ { id: Int => ResultRequest(id) }

  def command = {
    createPoll |
    list |
    deletePoll |
    startPoll |
    stopPoll |
    result
    // | {commandsWithContext}
  }

  def parse(string: String): Request = {
    CommandParser.parseAll(command, string).getOrElse(IllegalRequest(s"Bad command:\n$string"))
  }
}
