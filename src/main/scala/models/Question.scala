package models

import Commands.AddQuestion
import Types._

import scala.util.{Failure, Success, Try}

sealed trait Question {
  val name: String
  val options: List[String]
  val answered: List[User]
}

final case class OpenQuestion(name: String,
                        answers: List[OpenAnswer] = Nil,
                        answered: List[User] = Nil,
                        options: List[String] = Nil
                       ) extends Question

final case class ChoiceQuestion(name: String,
                          options: List[String],
                          answers: List[ChoiceAnswer] = Nil,
                          answered: List[User] = Nil
                         ) extends Question

final case class MultiQuestion(name: String,
                         options: List[String],
                         answered: List[User] = Nil,
                         answers: List[MultiAnswer] = Nil
                        ) extends Question

object Question {
  def fromCommand(r: AddQuestion): Question =
    r.qType match {
      case Open => OpenQuestion(r.question)
      case Choice => ChoiceQuestion(r.question, r.answers)
      case Multi => MultiQuestion(r.question, r.answers)
    }

  def tryAnswer(q: Question, u: User, anonymous: Boolean, s: String): Try[Question] =
    Try {
      q match {
        case q: OpenQuestion =>
          val a = tryOpen(s)
          a match {
            case Success(answer) =>
              if (anonymous)
                q.copy(answers = q.answers :+ (None, answer), answered = q.answered :+ u)
              else
                q.copy(answers = q.answers :+ (Option(u), answer), answered = q.answered :+ u)
            case Failure(e) => throw e
          }
        case q: ChoiceQuestion =>
          val a = tryChoice(s)
          assert(a.isSuccess, "Answer isn't successful!")
          a match {
            case Success(answer) =>
              assert(q.options.lift(answer).isDefined, "Answer isn't exists!")
              if (anonymous)
                q.copy(answers = q.answers :+ (None, answer), answered = q.answered :+ u)
              else
                q.copy(answers = q.answers :+ (Option(u), answer), answered = q.answered :+ u)
            case Failure(e) => throw e
          }
        case q: MultiQuestion =>
          val a = tryMulti(s)
          a match {
            case Success(answer) =>
              assert(answer.forall(a => q.options.lift(a).isDefined), "Answer isn't exists!")
              if (anonymous)
                q.copy(answers = q.answers :+ (None, answer), answered = q.answered :+ u)
              else
                q.copy(answers = q.answers :+ (Option(u), answer), answered = q.answered :+ u)
            case Failure(e) => throw e
          }
      }
    }

  private def tryOpen(s: String): Try[String] = Try(s)
  private def tryChoice(s: String): Try[Int] = Try(s.toInt)
  private def tryMulti(s: String): Try[Set[Int]] = Try {
    val a = s.split("\\s+").map(_.toInt)
    assert(a.length == a.distinct.length,
      "You can't vote twice!")
    a.toSet
  }
}