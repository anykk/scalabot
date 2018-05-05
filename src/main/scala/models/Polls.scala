package models

import java.time.LocalDateTime

import cats.data.StateT
import models.Types.User

import scala.util.Try

case class Polls(m: Map[Int, Poll], idx: Int = 1)

object Polls {
  import cats.implicits._

  def addPoll(p: Poll): StateT[Try, Polls, Int] = {
    for {
      _ <- StateT.modify[Try, Polls](ps => ps.copy(m = ps.m + (ps.idx -> p), idx = ps.idx + 1))
      s <- StateT.get[Try, Polls]
    } yield s.idx - 1
  }

  def deletePoll(id: Int): StateT[Try, Polls, Int] =
    StateT[Try, Polls, Int] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists!")
          val p = ps.m(id)
          assert(!Poll.isActive(LocalDateTime.now)(p), "Poll is active!")
          (ps.copy(m = ps.m - id), id)
        }
    }

  def startPoll(id: Int, u: User): StateT[Try, Polls, Int] =
    StateT[Try, Polls, Int] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists!")
          val p = ps.m(id)
          assert(Poll.haveRights(u)(p), "You haven't rights!")
          val n = LocalDateTime.now
          Poll.start(p)(n)
        }.flatMap(mp => mp.map(p => (ps.copy(m = ps.m + (id -> p)), id)))
    }

  def stopPoll(id: Int, u: User): StateT[Try, Polls, Int] =
    StateT[Try, Polls, Int] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists")
          val p = ps.m(id)
          assert(Poll.haveRights(u)(p), "You haven't rights")
          val n = LocalDateTime.now
          Poll.stop(p)(n)
        }.flatMap(mp => mp.map(p => (ps.copy(m = ps.m + (id -> p)), id)))
    }

  def pollResult(id: Int): StateT[Try, Polls, Poll] =
    StateT[Try, Polls, Poll] {
      ps =>
        Try {
          assert(ps.m.contains(id), "Poll doesn't exists")
          val p = ps.m(id)
          val n = LocalDateTime.now
          if (p.visibility)
            assert(Poll.isActive(n)(p), "Poll isn't active!")
          else
            assert(!Poll.isActive(n)(p), "Poll is active but visibility is afterstop!")
          (ps, p)
        }
    }
}
