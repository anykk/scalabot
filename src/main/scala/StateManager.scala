import scalaz.State
import Commands._
import Results.CommandResult
import models.{Contexts, Polls}
import models.Types.User

import scala.util.Try

object StateManager {
  type GS = (Polls, Contexts)

  val parser = CommandParser

  val performAction: User => String => Try[Command] => Try[State[GS, CommandResult]] =
    user =>
      msg =>
        parser.parse(msg) match {
          case r: CommonCommand => ???
          case r: ContextCommand => ???
        }

  private val simpleCommand:
}
