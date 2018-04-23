import java.time.LocalDateTime

package object Requests {
  sealed trait Request
  sealed trait CommonRequest extends Request

  case class CreatePollRequest(name: String,
                               anonymity: Boolean = true,
                               visibility: Boolean = false,
                               startTime: Option[LocalDateTime] = None,
                               stopTime: Option[LocalDateTime] = None) extends Request
  case class ListRequest() extends CommonRequest
  case class DeletePollRequest(id: Int) extends CommonRequest
  case class StartPollRequest(id: Int) extends CommonRequest
  case class StopPollRequest(id: Int) extends CommonRequest
  case class ResultRequest(id: Int) extends CommonRequest

  sealed trait ContextRequest extends Request

  case class BeginRequest(id: Int) extends ContextRequest
  case class EndRequest() extends ContextRequest
  case class ViewRequest() extends ContextRequest
  case class AddQuestionRequest(//TODO
                               ) extends ContextRequest
  case class DeleteQuestionRequest(id: Int) extends ContextRequest
  case class AnswerRequest(qnumber: Int, //TODO: answer
                          ) extends ContextRequest


  case class IllegalRequest(message: String) extends Request // with or without it?
}
