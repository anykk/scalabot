package models

import java.time.LocalDateTime

import Types.{GeneralState, User}
import cats.data.StateT

import scala.util.Try

final case class Contexts(m: Map[User, Int])

object Contexts {
  import cats.instances.try_.catsStdInstancesForTry
  import Assertions._

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
        _ <- assertIsActiveOrFinished(p, n)
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
        _ <- assertIsActiveOrFinished(p, n)
        _ <- assertQuestionExists(p, id)
      } yield (s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(questions =
            p.questions.patch(id, Nil, 1))))),
        Unit)
  }

  def answer(u: User, id: Int, n: LocalDateTime, answer: String): StateT[Try, GeneralState, Unit] = StateT {
    s =>
      for {
        _ <- assertNotIn(s._2, u)
        _ <- assertPollContains(s._1, s._2.m(u))
        i = s._2.m(u)
        p = s._1.m(i)
        _ <- assertIsNotActive(p, n)
        _ <- assertQuestionExists(p, id)
        q = p.questions(id)
        _ <- assertAlreadyAnswered(q, u)
      } yield (s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(
            questions = p.questions.updated(
              id, Question.tryAnswer(q, u, p.anonymity, answer).get))))),
        Unit)
  }
}
