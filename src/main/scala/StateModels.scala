import Models.Poll
import scalaz.State

package object StateModels {
  type Repository = Map[Int, Poll]
  type Contexts = Map[Int, Int]

  type PollsState = State[Repository, CommandResult]
  type ContextsState = StateModels[Contexts, CommandResult] // ?

  def createPoll()
}
