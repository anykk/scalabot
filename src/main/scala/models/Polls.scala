package models

import java.time.LocalDateTime

import scalaz.State

import scala.util.Try

case class Polls(s: Map[Int, Poll], i: Int = 1)

object Polls {
  type PS = State[Polls, Int]

  def addPoll(p: Poll): PS =
    for {
      _ <- State.modify[Polls](ps => ps.copy(s = ps.s + (ps.i -> p), i = ps.i + 1))
      s <- State.get[Polls]
    } yield s.i

  def deletePoll(i: Int): Try[PS] = Try {
    State[Polls, Int](
      ps => {
        assert(ps.s.contains(i), "Poll doesn't exists!")
        val p = ps.s(i)
        assert(!Poll.isActive(LocalDateTime.now)(p), "Poll is active!")
        (ps.copy(s = ps.s - i), i)
      }
    )
  }

}
