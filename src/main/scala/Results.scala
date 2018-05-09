import models.Poll

package object Results {
  sealed trait CommandResult

  case class PollCreated(id: Int) extends CommandResult
  case class ListResult(listing: List[(Int, Poll)]) extends CommandResult
  case class PollDeleted() extends CommandResult
  case class PollStarted() extends CommandResult
  case class PollStopped() extends CommandResult
  case class PollingResult(p: Poll) extends CommandResult

  case class BeginResult() extends CommandResult
  case class EndResult() extends CommandResult
  case class ViewResult(p: Poll) extends CommandResult
  case class QuestionAdded(id: Int) extends CommandResult
  case class QuestionDeleted() extends CommandResult
  case class AnswerResult() extends CommandResult
}
