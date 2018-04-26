import java.time.LocalDateTime


package object Commands {
  sealed trait Command
  sealed trait CommonCommand extends Command

  case class CreatePollCommand(name: String,
                               anonymity: Boolean = true,
                               visibility: Boolean = false,
                               startTime: Option[LocalDateTime] = None,
                               stopTime: Option[LocalDateTime] = None) extends Command
  case class ListCommand() extends CommonCommand
  case class DeletePollCommand(id: Int) extends CommonCommand
  case class StartPollCommand(id: Int) extends CommonCommand
  case class StopPollCommand(id: Int) extends CommonCommand
  case class ResultCommand(id: Int) extends CommonCommand

  sealed trait ContextCommand extends Command

  case class BeginCommand(id: Int) extends ContextCommand
  case class EndCommand() extends ContextCommand
  case class ViewCommand() extends ContextCommand
  case class AddQuestionCommand(question: String, answers: List[String],
                                qType: QType = Open) extends ContextCommand
  case class DeleteQuestionCommand(id: Int) extends ContextCommand
  case class AnswerCommand(id: Int, answer: String) extends ContextCommand


  case class IllegalCommand(message: String) extends Command

  sealed trait QType
  case object Open extends QType
  case object Choice extends QType
  case object Multi extends QType
}
