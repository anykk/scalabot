package models

import java.time.LocalDateTime

import Types.{GeneralState, User}
import cats.data.StateT

import scala.util.Try

final case class Contexts(m: Map[User, Int])

object Contexts {
  import cats.implicits.catsStdInstancesForTry

  def alreadyIn(c: Contexts, u: User): Boolean = c.m.getOrElse(u, -1) != -1

  def begin(u: User, id: Int): StateT[Try, GeneralState, Unit] = StateT {
    s => Try {
      assert(!alreadyIn(s._2, u), "Already in context!")
      assert(s._1.m.contains(id), "Poll with this id doesn't exist!")
      ( s.copy(_2 = s._2.copy(m = s._2.m + (u -> id))), Unit )
    }
  }

  def end(u: User): StateT[Try, Contexts, Unit] = StateT {
    s => Try {
      assert(alreadyIn(s, u),
        "Not in context!")
      ( s.copy(m = s.m - u), Unit )
    }
  }

  def view(u: User): StateT[Try, GeneralState, Poll] = StateT {
    s => Try {
      assert(alreadyIn(s._2, u),
        "Not in context!")
      assert(s._1.m.contains(s._2.m(u)),
        "Poll was deleted! Sorry, but please go end.")
      ( s.copy(), s._1.m(s._2.m(u)) )
    }
  }

  def addQuestion(u: User, q: Question, n: LocalDateTime): StateT[Try, GeneralState, Int] = StateT {
    s => Try {
      assert(alreadyIn(s._2, u),
        "Not in context!")
      assert(s._1.m.contains(s._2.m(u)),
        "Poll was deleted! Sorry, but please go end.")
      val i = s._2.m(u)
      val p = s._1.m(i)
      assert(Poll.haveRights(u, p),
        "You haven't rights!")
      assert(!Poll.isActive(p, n) && !Poll.isStopped(p, n),
        "Poll is active or already finished now!")
      ( s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(questions =
            p.questions :+ q))) ),
        s._1.m(i).questions.length )
    }
  }

  def deleteQuestion(u: User, id: Int, n: LocalDateTime): StateT[Try, GeneralState, Unit] = StateT {
    s => Try {
      assert(alreadyIn(s._2, u),
        "Not in context!")
      assert(s._1.m.contains(s._2.m(u)),
        "Poll was deleted! Sorry, but please go end.")
      val i = s._2.m(u)
      val p = s._1.m(i)
      assert(Poll.haveRights(u, p),
        "You haven't rights!")
      assert(!Poll.isActive(p, n) && !Poll.isStopped(p, n),
        "Poll is active or already finished now!")
      assert(p.questions.length >= id,
        "Question with this id doesn't exists!")
      ( s.copy(_1 =
        s._1.copy(m =
          s._1.m updated(i, p.copy(questions =
          p.questions.patch(id, Nil, 1)))
      )), Unit)
    }
  }

}
