package models

import java.time.LocalDateTime

import cats.data.StateT
import models.Types.User

import scala.util.Try

final case class Polls(m: Map[Int, Poll], idx: Int = 1)

object Polls {
  import cats.instances.try_._

  def addPoll(p: Poll): StateT[Try, Polls, Int] = {
    for {
      _ <- StateT.modify[Try, Polls](ps => ps.copy(m = ps.m + (ps.idx -> p), idx = ps.idx + 1))
      s <- StateT.get[Try, Polls]
    } yield s.idx - 1
  }

  def deletePoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists!")
          val p = ps.m(id)
          assert(Poll.haveRights(u, p), "You haven't rights!")
          assert(!Poll.isActive(p, n), "Poll is active!")
          ( ps.copy(m = ps.m - id), Unit )
        }
    }

  def startPoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists!")
          val p = ps.m(id)
          assert(Poll.haveRights(u, p), "You haven't rights!")
          Poll.start(p, n)
        }.flatMap(mp => mp.map(p => (ps.copy(m = ps.m + (id -> p)), Unit)))
    }

  def stopPoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists")
          val p = ps.m(id)
          assert(Poll.haveRights(u, p), "You haven't rights")
          Poll.stop(p, n)
        }.flatMap(mp => mp.map(p => ( ps.copy(m = ps.m + (id -> p)), Unit )))
    }

  def pollResult(id: Int, n: LocalDateTime): StateT[Try, Polls, Poll] =
    StateT[Try, Polls, Poll] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists")
          val p = ps.m(id)
          assert(Poll.isVisible(p, n), "Can't show result of this poll!")
          (ps, p)
        }
    }
}
