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
                  questions: Vector[Question])

  object Poll {
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

  case class Polls(map: Map[Int, Poll])

}
