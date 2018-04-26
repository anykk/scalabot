import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import scala.util.Try
import scala.util.parsing.combinator._
import Commands._


trait CommandParser {
  def parse(string: String): Command
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
    "(" ~> rep(leftBracket | rightBracket | string) <~ ")" ^^ (xs => xs.mkString(sep = " "))

  def digitArgument: Parser[Int] = "(" ~> digits <~ ")"

  def dateArgument: Parser[Option[LocalDateTime]] = "(" ~> date <~ ")"

  def createPoll: Parser[CreatePollCommand] =
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
        CreatePollCommand(name)
      case name ~ Some(anonymity ~ None) =>
        CreatePollCommand(name, anonymity)
      case name ~ Some(anonymity ~ Some(visibility ~ None)) =>
        CreatePollCommand(name, anonymity, visibility)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ None))) =>
        CreatePollCommand(name, anonymity, visibility, startTime)
      case name ~ Some(anonymity ~ Some(visibility ~ Some(startTime ~ Some(endTime)))) =>
        CreatePollCommand(name, anonymity, visibility, startTime, endTime)
    }

  def list: Parser[ListCommand] = "/list".r ^^^ {
    ListCommand()
  }

  private def commandWithId: String => Parser[Int] = (commandName: String) =>
    commandName.r ~> digitArgument

  def deletePoll: Parser[DeletePollCommand] = commandWithId("/delete_poll") ^^ {
    id: Int => DeletePollCommand(id)
  }

  def startPoll: Parser[StartPollCommand] = commandWithId("/start_poll") ^^ {
    id: Int => StartPollCommand(id)
  }

  def stopPoll: Parser[StopPollCommand] = commandWithId("/stop_poll") ^^ {
    id: Int => StopPollCommand(id)
  }

  def result: Parser[ResultCommand] = commandWithId("/result") ^^ {
    id: Int => ResultCommand(id)
  }

  def begin: Parser[BeginCommand] = commandWithId("/begin") ^^ {
    id: Int => BeginCommand(id)
  }

  def end: Parser[EndCommand] = "/end".r ^^^ {
    EndCommand()
  }

  def view: Parser[ViewCommand] = "/view".r ^^^ {
    ViewCommand()
  }

  private def qType: Parser[QType] = opt("(" ~> "open|multi|choice".r <~ ")") ^^ {
    case Some(s) => s match {
      case "open" => Open
      case "multi" => Multi
      case "choice" => Choice
    }
    case None => Open
  }

  def addQuestion: Parser[AddQuestionCommand] = "/add_question".r ~>
    stringArgument ~
    qType ~
    rep(stringArgument) ^^ {???}/*{
    case question ~ qType ~ answers if answers.isEmpty => qType match {
      case Open => AddQuestionCommand(question, answers, qType)
    }
    case question ~ qType ~ answers if answers.nonEmpty => qType match {
      case Multi => AddQuestionCommand(question, answers, qType)
      case Choice => AddQuestionCommand(question, answers, qType)
    }

  }*/


  def deleteQuestion: Parser[DeleteQuestionCommand] = commandWithId("/deleteQuestion") ^^ {
    id: Int => DeleteQuestionCommand(id)
  }

  def answer: Parser[AnswerCommand] = "/answer".r ~> digitArgument ~ stringArgument ^^ {
    case id ~ answer => AnswerCommand(id, answer)
  }

  def command: Parser[Command] = {
    createPoll |
      list |
      deletePoll |
      startPoll |
      stopPoll |
      result |
      begin |
      end |
      view |
      addQuestion |
      deleteQuestion |
      answer
  }

  def parse(string: String): Command = {
    CommandParser.parseAll(command, string) match {
      case Success(x, _) => x
      case NoSuccess(_) => IllegalCommand(string)
    }
  }
}