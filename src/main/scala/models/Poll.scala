package models

import java.time.LocalDateTime
import scala.util.Try
import models.Types.User

case class Poll(name: String,
                anonymity: Boolean,
                visibility: Boolean,
                startTime: Option[LocalDateTime],
                stopTime: Option[LocalDateTime],
                owner: User,
                questions: List[Question]
               )

object Poll {
  def haveRights(u: User)(mp: Option[Poll]): Boolean = mp match {
    case Some(v) => v.owner == u
    case None => false
  }

  def startPoll(mp: Option[Poll])(n: LocalDateTime) = Try {
    assert(couldStart(mp), "Couldn't start this poll!")
    mp.get.copy(startTime = Option(n))
  }

  def stopPoll(mp: Option[Poll])(n: LocalDateTime) = Try {
    assert(couldStop(mp), "Couldn't stop this poll!")
    mp.get.copy(stopTime = Option(n))
  }

  def couldStart(mp: Option[Poll]): Boolean = mp match {
    case Some(p) =>
      p.startTime match {
        case None => true
        case Some(_) => false
      }
    case None => false
  }

  def couldStop(mp: Option[Poll]): Boolean = mp match {
    case Some(p) =>
      p.stopTime match {
        case Some(_) => false
        case None if Poll.isStarted(LocalDateTime.now)(p) => true
        case _ => false
      }
    case None => false
  }

  def isStarted(now: LocalDateTime)(poll: Poll): Boolean =
    poll.startTime.exists(now.isAfter)

  def isStopped(now: LocalDateTime)(poll: Poll): Boolean  =
    poll.stopTime.exists(now.isAfter)

  def isActive(now: LocalDateTime)(poll: Poll): Boolean =
    isStarted(now)(poll) && !isStopped(now)(poll)

  def isVisible(now: LocalDateTime)(poll: Poll): Boolean =
    poll.visibility && isStarted(now)(poll) || !poll.visibility && isStopped(now)(poll)
}