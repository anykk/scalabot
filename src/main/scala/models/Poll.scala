package models

import java.time.LocalDateTime
import scala.util.Try
import models.Types.User

final case class Poll(name: String,
                anonymity: Boolean,
                visibility: Boolean,
                startTime: Option[LocalDateTime],
                stopTime: Option[LocalDateTime],
                owner: User,
                questions: Vector[Question]
               )

object Poll {
  def haveRights(u: User)(p: Poll): Boolean = p.owner == u

  def start(p: Poll, n: LocalDateTime) = Try {
    assert(couldStart(p), "Couldn't start this poll!")
    p.copy(startTime = Option(n))
  }

  def stop(p: Poll, n: LocalDateTime) = Try {
    assert(couldStop(p, n), "Couldn't stop this poll!")
    p.copy(stopTime = Option(n))
  }

  def couldStart(p: Poll): Boolean =
    p.startTime match {
      case None => true
      case Some(_) => false
    }

  def couldStop(p: Poll, n: LocalDateTime): Boolean =
    p.stopTime match {
      case Some(_) => false
      case None if Poll.isStarted(p, n) => true
      case _ => false
    }

  def isStarted(p: Poll, n: LocalDateTime): Boolean =
    p.startTime.exists(n.isAfter)

  def isStopped(p: Poll, n: LocalDateTime): Boolean  =
    p.stopTime.exists(n.isAfter)

  def isActive(p: Poll, n: LocalDateTime): Boolean =
    isStarted(p, n) && !isStopped(p, n)

  def isVisible(p: Poll, n: LocalDateTime): Boolean =
    p.visibility && isStarted(p, n) || !p.visibility && isStopped(p, n)
}