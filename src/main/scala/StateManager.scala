import Commands.Command
import models.Polls
import models.Types.User
import scalaz.State

import scala.util.Try

object StateManager {
  val performAction: User => Try[Command] => State[Polls, Results.CommandResult] = ???
}
