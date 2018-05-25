import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import Results._
import models._

object Render {
  private val anonymityMap = Map(true -> "yes", false -> "no")
  private val visibilityMap = Map(true -> "continuous", false -> "afterstop")

  def apply(result: CommandResult): String =
    result match {
      case PollCreated(i) => s"Poll with $i id was successfully created."
      case ListResult(l) => viewList(l)
      case PollDeleted() => "Poll was successfully deleted."
      case PollStarted() => "Poll was successfully started."
      case PollStopped() => "Poll was successfully stopped."
      case PollingResult(p) => viewResult(p)
      case BeginResult() => "Context was successfully changed."
      case EndResult() => "You left the context."
      case ViewResult(p) => viewPoll(p)
      case QuestionAdded(i) => s"Question with $i id was successfully added."
      case QuestionDeleted() => "Question was successfully deleted."
      case AnswerResult() => "Answer was accepted."
    }

  def viewList(l: List[(Int, Poll)]): String = {
    val r = l.map(p => p._1 + s""": "${p._2.name}"""")
    if (r.isEmpty) "Empty." else r.mkString("\n")
  }

  private val dateFormat = DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd")

  private def date(d: Option[LocalDateTime]): String =
    d match{
      case Some(x) => dateFormat.format(x)
      case None => "Not set"
    }

  def viewPoll(p: Poll): String =
  s"""Poll "${p.name}":
     | Anonymity: ${anonymityMap(p.anonymity)}
     | Visibility: ${visibilityMap(p.visibility)}
     | Start time: ${date(p.startTime)}
     | Stop time: ${date(p.stopTime)}
     | Questions:
     |${p.questions.zipWithIndex.map(q => viewQuestion(q._1, q._2)).mkString("\n")
  }
  """.stripMargin

  private def qType(q: Question): String =
    q match {
      case _: OpenQuestion => "Open"
      case _: MultiQuestion => "Multi"
      case _: ChoiceQuestion => "Choice"
    }

  private def viewQuestion(q: Question, id: Int): String = {
    val options =
      if (q.options.isEmpty)
        "Empty."
      else
        q.options.zipWithIndex.map(o => s"""    ${o._2}: "${o._1}"""").mkString("\n")
    s"""Question [$id]
       |   Name: "${q.name}"
       |   Type: ${qType(q)}
       |   Options:
       |$options
     """.stripMargin
  }

  private def percent(q: ChoiceQuestion, id: Int): Int =
    (q.answers.count(_._2 == id) / q.answered.length.toFloat * 100).toInt

  private def percent(q: MultiQuestion, id: Int): Int =
    (q.answers.count(_._2.contains(id)) / q.answered.length.toFloat * 100).toInt

  private def percent(q: OpenQuestion, answer: String): Int =
    (q.answers.count(_._2 == answer) / q.answered.length.toFloat * 100).toInt

  private def choiceResult(q: ChoiceQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      s"""    "${i._1}": ${percent(q, i._2)}%""").mkString("\n")}"""

  private def multiResult(q: MultiQuestion): String =
    s"""${q.options.zipWithIndex.map(i =>
      s"""    "${i._1}": ${percent(q, i._2)}%""").mkString("\n")}"""

  private def openResult(q: OpenQuestion): String =
    s"""${q.answers.map(_._2).toSet[String].map(s =>
      s"""    "$s": ${percent(q, s)}%""").mkString("\n")}"""

  def viewResult(p: Poll): String =
    s"""Poll "${p.name}":
       |${p.questions.map(questionResult).mkString("\n")}"""

  private def questionResult(q: Question): String =   s"""  ${q.name}" [${qType(q)}]:\n""" +
    (q match {
      case x: ChoiceQuestion => choiceResult(x)
      case x: MultiQuestion => multiResult(x)
      case x: OpenQuestion => openResult(x)
    })
}
