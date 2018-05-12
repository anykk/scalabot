import Results._
import models.{Poll, Question}

object Render {
  def apply(result: CommandResult): String =
    result match {
      case PollCreated(i) => s"Poll with $i id was successfully created."
      case ListResult(l) => "listing..."
      case PollDeleted() => "Poll was successfully deleted."
      case PollStarted() => "Poll was successfully started."
      case PollStopped() => "Poll was successfully stopped."
      case PollingResult(p) => "result..."
      case BeginResult() => "Context was successfully changed."
      case EndResult() => "You left the context."
      case ViewResult(p) => "view..."
      case QuestionAdded(i) => s"Question with $i id was successfully added."
      case QuestionDeleted() => "Question was successfully deleted."
      case AnswerResult() => "Answer was accepted."
    }

  private def renderPoll(p: Poll): String = ???
  private def renderQuestion(q: Question): String = ???
}
