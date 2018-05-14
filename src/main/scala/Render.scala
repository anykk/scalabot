import Results._
import models._

object Render {
  private val anonymityMap = Map(true -> "continuous", false -> "afterstop")
  private val visibilityMap = Map(true -> "yes", false -> "no")

  def apply(result: CommandResult): String =
    result match {
      case PollCreated(i) => s"Poll with $i id was successfully created."
      case ListResult(l) => l.mkString("\n") // TODO: normal view
      case PollDeleted() => "Poll was successfully deleted."
      case PollStarted() => "Poll was successfully started."
      case PollStopped() => "Poll was successfully stopped."
      case PollingResult(p) => "result..." // TODO: result with percents
      case BeginResult() => "Context was successfully changed."
      case EndResult() => "You left the context."
      case ViewResult(p) => viewPoll(p)
      case QuestionAdded(i) => s"Question with $i id was successfully added."
      case QuestionDeleted() => "Question was successfully deleted."
      case AnswerResult() => "Answer was accepted."
    }

  private def viewPoll(p: Poll): String =
  s"""
    |Name: ${p.name}
    |Anonymity: ${anonymityMap(p.anonymity)}
    |Visibility: ${visibilityMap(p.visibility)}
    |Start time: ${p.startTime.getOrElse("None")}
    |Stop time: ${p.stopTime.getOrElse("None")}
    |Questions: ${p.questions.map(viewQuestion).mkString("\n")}
  """.stripMargin

  private def viewQuestion(q: Question): String = {
    s"""
       |Name: ${q.name}
       |Options: ${q.options.mkString("\n")}
     """.stripMargin
  }
}
