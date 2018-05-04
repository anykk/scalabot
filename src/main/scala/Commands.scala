import java.time.LocalDateTime

import models.Types.{QuestionType, Open}


package object Commands {
  sealed trait Command
  sealed trait CommonCommand extends Command

  case class CreatePoll(name: String,
                        anonymity: Boolean = true,
                        visibility: Boolean = false,
                        startTime: Option[LocalDateTime] = None,
                        stopTime: Option[LocalDateTime] = None
                       ) extends Command
  case class List_() extends CommonCommand
  case class DeletePoll(id: Int) extends CommonCommand
  case class StartPoll(id: Int) extends CommonCommand
  case class StopPoll(id: Int) extends CommonCommand
  case class Result(id: Int) extends CommonCommand

  sealed trait ContextCommand extends Command

  case class Begin(id: Int) extends ContextCommand
  case class End() extends ContextCommand
  case class View() extends ContextCommand

  case class AddQuestion(question: String, answers: List[String],
                         qType: QuestionType = Open) extends ContextCommand
  case class DeleteQuestion(id: Int) extends ContextCommand
  case class Answer(id: Int, answers: String) extends ContextCommand
}
