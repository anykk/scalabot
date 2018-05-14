package models

import java.time.LocalDateTime

import Types.{GeneralState, User}
import cats.data.StateT

import scala.util.Try

final case class Contexts(m: Map[User, Int])

object Contexts {
  import cats.instances.try_._

  private def isInContext(c: Contexts, u: User): Boolean =
    c.m.getOrElse(u, -1) != -1

  private def assertIn(c: Contexts, u: User): Try[Unit] =
    Try { assert(!isInContext(c, u), "Already in context!") }

  private def assertNotIn(c: Contexts, u: User): Try[Unit] =
    Try { assert(isInContext(c, u), "Not in context!") }

  private def assertPollContains(c: Polls, id: Int): Try[Unit] =
    Try { assert(c.m.contains(id), "Poll with this id doesn't exist!") }

  private def assertHaveRights(p: Poll, u: User): Try[Unit] =
    Try { assert(Poll.haveRights(u, p), "You haven't rights!") }

  private def assertIsActive(p: Poll, n: LocalDateTime): Try[Unit] =
    Try { assert(!Poll.isActive(p, n) && !Poll.isStopped(p, n),
      "Poll is active or already finished now!") }

  private def assertQuestionExists(p: Poll, id: Int): Try[Unit] =
    Try { assert(id < p.questions.length,
      "Question with this id doesn't exists!") }

  def begin(u: User, id: Int): StateT[Try, GeneralState, Unit] = StateT {
    s =>
      for {
        _ <- assertIn(s._2, u)
        _ <- assertPollContains(s._1, id)
      } yield (s.copy(_2 = s._2.copy(m = s._2.m + (u -> id))), Unit)
  }

  def end(u: User): StateT[Try, Contexts, Unit] = StateT {
    s =>
      for {
        _ <- assertNotIn(s, u)
      } yield (s.copy(m = s.m - u), Unit)
  }

  def view(u: User): StateT[Try, GeneralState, Poll] = StateT {
    s =>
      for {
        _ <- assertNotIn(s._2, u)
        _ <- assertPollContains(s._1, s._2.m(u))
      } yield (s.copy(), s._1.m(s._2.m(u)))
  }

  def addQuestion(u: User, q: Question, n: LocalDateTime): StateT[Try, GeneralState, Int] = StateT {
    s =>
      for {
        _ <- assertNotIn(s._2, u)
        _ <- assertPollContains(s._1, s._2.m(u))
        i = s._2.m(u)
        p = s._1.m(i)
        _ <- assertHaveRights(p, u)
        _ <- assertIsActive(p, n)
      } yield (s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(questions =
            p.questions :+ q)))),
        s._1.m(i).questions.length)
  }

  def deleteQuestion(u: User, id: Int, n: LocalDateTime): StateT[Try, GeneralState, Unit] = StateT {
    s =>
      for {
        _ <- assertNotIn(s._2, u)
        _ <- assertPollContains(s._1, s._2.m(u))
        i = s._2.m(u)
        p = s._1.m(i)
        _ <- assertHaveRights(p, u)
        _ <- assertIsActive(p, n)
        _ <- assertQuestionExists(p, id)
      } yield (s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(questions =
            p.questions.patch(id, Nil, 1))))),
        Unit)
  }

  def answer(u: User, id: Int, n: LocalDateTime, answer: String): StateT[Try, GeneralState, Unit] = StateT {
    s => Try {
      assert(isInContext(s._2, u),
        "Not in context!")
      assert(s._1.m.contains(s._2.m(u)),
        "Poll was deleted! Sorry, but please go end.")
      val i = s._2.m(u)
      val p = s._1.m(i)
      assert(Poll.isActive(p, n),
        "Poll isn't active now!!")
      assert(id < p.questions.length,
        "Question with this id doesn't exists!")
      val q = p.questions(id)
      assert(!q.answered.contains(u), "You has already answered!")
      ( s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(
            questions = p.questions.updated(id, Question.tryAnswer(q, u, p.anonymity, answer).get))))), Unit )
    }
  }
}
