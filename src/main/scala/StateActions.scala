import Models.{Poll, Polls}
import Results.CreatePollResult
import scalaz.State

package object StateActions {
  private val id = Stream.from(1).iterator

  def addPoll(poll: Poll): State[Polls, Int] = State { (s: Polls) =>
    val id = this.id.next()
    (s.copy(map = s.map + (id -> poll)), id)
  }

  def deletePoll(id: Int): State[Polls, Int] = State { (s: Polls) =>
    val poll = s.map.get(id)
    poll match {
      case Some(_) => (s.copy(map = s.map - id), id)
      case None => (s, -1)
    }
  }
}
