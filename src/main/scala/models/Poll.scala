package models

import java.time.LocalDateTime

case class Poll(name: String,
                anonymity: Boolean,
                visibility: Boolean,
                startTime: Option[LocalDateTime],
                stopTime: Option[LocalDateTime],
                owner: Long,
                questions: List[Question]
               )

object Poll {


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