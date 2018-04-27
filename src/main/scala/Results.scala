import Models.Poll

package object Results {
  sealed trait CommandResult

  case class CreatePollResult(id: Int) extends CommandResult
  case class ListResult(listing: List[Poll]) extends CommandResult
  case class DeletePollResult(id: Int) extends CommandResult
  case class StartPollResult(id: Int) extends CommandResult
  case class StopPollResult(id: Int) extends CommandResult
  case class ResultResult(id: Int) extends CommandResult //LULZ

  case class BeginResult(id: Int) extends CommandResult
  case class EndResult() extends CommandResult
  case class ViewResult() extends CommandResult //??
  case class AddQuestionResult(id: Int) extends CommandResult
  case class DeleteQuestionResult(id: Int) extends CommandResult
  case class AnswerResult() extends CommandResult //??

  //Result[A] (value: Option[A]) ???
  /*
  Result(v) match {
    case Some(r) => r
    case None => bad(Result(v))
  }
   */
}
