package models

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
      assert(alreadyIn(s, u), "Not in context!")
      ( s.copy(m = s.m - u), Unit )
    }
  }

  def view(u: User): StateT[Try, GeneralState, Poll] = StateT {
    s => Try {
      assert(alreadyIn(s._2, u), "Not in context!")
      ( s.copy(), s._1.m(s._2.m(u)) )
    }
  }
}
