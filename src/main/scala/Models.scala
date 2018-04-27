import java.time.LocalDateTime
import scalaz.State


package object Models {
  type User = Int

  case class Poll(name: String,
                  owner: User,
                  anonymity: Boolean,
                  visibility: Boolean,
                  startTime: Option[LocalDateTime],
                  stopTime: Option[LocalDateTime],
                  questions: List[Question])

  object Poll {
    def couldStart(poll: Poll, now: LocalDateTime): Boolean = {
      poll.startTime match {
        case None => true
        case Some(_) => false
      }
    }

    def couldStop(poll: Poll, now: LocalDateTime): Boolean = {
      poll.stopTime match {
        case None => true
        case Some(_) => false
      }
    }

    def isStarted(poll: Poll, now: LocalDateTime): Boolean =
      poll.startTime.exists(now.isAfter)

    def isStopped(poll: Poll, now: LocalDateTime): Boolean =
      poll.stopTime.exists(now.isAfter)

    def isActive(poll: Poll, now: LocalDateTime): Boolean =
      isStarted(poll, now) && !isStopped(poll, now)

    def isVisible(poll: Poll, now: LocalDateTime): Boolean =
      poll.visibility && isStarted(poll, now) || !poll.visibility && isStopped(poll, now)
  }

  sealed trait Question
  sealed trait Answer
  //TODO: this two ^

  case class Polls(map: Map[Int, Poll])

  object Polls {
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

  case class Contexts(map: Map[Int, Int])

}
