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
                questions: Vector[Question]
               )

object Poll {
  def haveRights(u: User)(p: Poll): Boolean = p.owner == u

  def start(p: Poll)(n: LocalDateTime) = Try {
    assert(couldStart(p), "Couldn't start this poll!")
    p.copy(startTime = Option(n))
  }

  def stop(p: Poll)(n: LocalDateTime) = Try {
    assert(couldStop(p), "Couldn't stop this poll!")
    p.copy(stopTime = Option(n))
  }

  def couldStart(p: Poll): Boolean =
    p.startTime match {
      case None => true
      case Some(_) => false
    }

  def couldStop(p: Poll): Boolean =
    p.stopTime match {
      case Some(_) => false
      case None if Poll.isStarted(LocalDateTime.now)(p) => true
      case _ => false
    }

  def isStarted(n: LocalDateTime)(p: Poll): Boolean =
    p.startTime.exists(n.isAfter)

  def isStopped(n: LocalDateTime)(p: Poll): Boolean  =
    p.stopTime.exists(n.isAfter)

  def isActive(n: LocalDateTime)(p: Poll): Boolean =
    isStarted(n)(p) && !isStopped(n)(p)

  def isVisible(n: LocalDateTime)(p: Poll): Boolean =
    p.visibility && isStarted(n)(p) || !p.visibility && isStopped(n)(p)
}