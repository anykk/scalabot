package models

import scala.util.Try
import java.time.LocalDateTime
import models.Types.User

object Assertions {
  def isInContext(c: Contexts, u: User): Boolean =
    c.m.getOrElse(u, -1) != -1

  def assertIn(c: Contexts, u: User): Try[Unit] =
    Try { assert(!isInContext(c, u), "Already in context!") }

  def assertNotIn(c: Contexts, u: User): Try[Unit] =
    Try { assert(isInContext(c, u), "Not in context!") }

  def assertPollContains(c: Polls, id: Int): Try[Unit] =
    Try { assert(c.m.contains(id), "Poll with this id doesn't exist!") }

  def assertIsNotVisible(p: Poll, n: LocalDateTime): Try[Unit] =
    Try { assert(Poll.isVisible(p, n), "Can't show result of this poll!") }

  def assertHaveRights(p: Poll, u: User): Try[Unit] =
    Try { assert(Poll.haveRights(u, p), "You haven't rights!") }

  def assertIsActive(p: Poll, n: LocalDateTime): Try[Unit] =
    Try { assert(!Poll.isActive(p, n), "Poll is active now!") }

  def assertIsActiveOrFinished(p: Poll, n: LocalDateTime): Try[Unit] =
    Try { assert(!Poll.isActive(p, n) && !Poll.isStopped(p, n),
      "Poll is active or already finished now!") }

  def assertIsNotActive(p: Poll, n: LocalDateTime): Try[Unit] =
    Try { assert(Poll.isActive(p, n), "Poll isn't active now!") }

  def assertQuestionExists(p: Poll, id: Int): Try[Unit] =
    Try { assert(id < p.questions.length,
      "Question with this id doesn't exists!") }

  def assertAlreadyAnswered(q: Question, u: User): Try[Unit] =
    Try { assert(!q.answered.contains(u), "You has already answered!") }
}
