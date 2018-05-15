import Results._
import models._

object Render {
  private val anonymityMap = Map(true -> "continuous", false -> "afterstop")
  private val visibilityMap = Map(true -> "yes", false -> "no")

  def apply(result: CommandResult): String =
    result match {
      case PollCreated(i) => s"Poll with $i id was successfully created."
      case ListResult(l) => viewList(l)
      case PollDeleted() => "Poll was successfully deleted."
      case PollStarted() => "Poll was successfully started."
      case PollStopped() => "Poll was successfully stopped."
      case PollingResult(p) => p.toString // TODO: result with percents
      case BeginResult() => "Context was successfully changed."
      case EndResult() => "You left the context."
      case ViewResult(p) => viewPoll(p)
      case QuestionAdded(i) => s"Question with $i id was successfully added."
      case QuestionDeleted() => "Question was successfully deleted."
      case AnswerResult() => "Answer was accepted."
    }

  private def viewList(l: List[(Int, Poll)]): String = {
    val r = l.map(p => p._1 + s": '${p._2.name}'")
    if (r.isEmpty) "Empty!" else r.mkString("\n")
  }

  private def viewPoll(p: Poll): String =
  s"""Poll:
     |  Name: ${p.name}
     |  Anonymity: ${anonymityMap(p.anonymity)}
     |  Visibility: ${visibilityMap(p.visibility)}
     |  Start time: ${p.startTime.getOrElse("Not set")}
     |  Stop time: ${p.stopTime.getOrElse("Not set")}
     |  Questions:
     |    ${p.questions.map(q => q.name + s" (${qType(q)})").mkString("\n    ")}
  """.stripMargin

  private def qType(q: Question): String =
    q match {
      case _: OpenQuestion => "Open"
      case _: MultiQuestion => "Multi"
      case _: ChoiceQuestion => "Choice"
    }

  private def viewQuestion(q: Question): String = {
    val options = if (q.options.isEmpty) "Options is empty!" else q.options.mkString("\n")
    s"""Name: ${q.name}
       |Type: ${qType(q)}
       |Options:
       | $options
     """.stripMargin
  }
}
