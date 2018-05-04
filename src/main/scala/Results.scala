import models.Poll

package object Results {
  sealed trait CommandResult

  case class PollCreated(id: Int) extends CommandResult
  case class ListResult(listing: List[Poll]) extends CommandResult
  case class PollDeleted(id: Int) extends CommandResult
  case class PollStarted(id: Int) extends CommandResult
  case class PollStopped(id: Int) extends CommandResult
  case class PollingResult(id: Int) extends CommandResult

  case class BeginResult(id: Int) extends CommandResult
  case class EndResult() extends CommandResult
  case class ViewResult(p: Poll) extends CommandResult
  case class QuestionAdded(id: Int) extends CommandResult
  case class QuestionDeleted(id: Int) extends CommandResult
  case class AnswerResult() extends CommandResult
}
