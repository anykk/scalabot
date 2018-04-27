import java.time.LocalDateTime


package object Commands {
  sealed trait Command
  sealed trait CommonCommand extends Command

  case class CreatePoll(name: String,
                        anonymity: Boolean = true,
                        visibility: Boolean = false,
                        startTime: Option[LocalDateTime] = None,
                        stopTime: Option[LocalDateTime] = None) extends Command
  case class List() extends CommonCommand
  case class DeletePoll(id: Int) extends CommonCommand
  case class StartPoll(id: Int) extends CommonCommand
  case class StopPoll(id: Int) extends CommonCommand
  case class Result(id: Int) extends CommonCommand

  sealed trait ContextCommand extends Command

  case class Begin(id: Int) extends ContextCommand
  case class End() extends ContextCommand
  case class View() extends ContextCommand
  case class AddQuestion(question: String, answers: List[String],
                         qType: QType = Open) extends ContextCommand
  case class DeleteQuestion(id: Int) extends ContextCommand
  case class Answer(id: Int, answer: String) extends ContextCommand


  case object Unrecognized extends Command

  sealed trait QType
  case object Open extends QType
  case object Choice extends QType
  case object Multi extends QType
}
