package models

import java.time.LocalDateTime

import cats.data.StateT
import models.Types.User

import scala.util.Try

final case class Polls(m: Map[Int, Poll], idx: Int = 1)

object Polls {

  import cats.instances.try_._
  import Assertions._

  def addPoll(p: Poll): StateT[Try, Polls, Int] = {
    for {
      _ <- StateT.modify[Try, Polls](ps => ps.copy(m = ps.m + (ps.idx -> p), idx = ps.idx + 1))
      s <- StateT.get[Try, Polls]
    } yield s.idx - 1
  }

  def deletePoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        for {
          _ <- assertPollContains(ps, id)
          p = ps.m(id)
          _ <- assertHaveRights(p, u)
          _ <- assertIsActive(p, n)
        } yield (ps.copy(m = ps.m - id), Unit)
    }

  def startPoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        for {
          _ <- assertPollContains(ps, id)
          p = ps.m(id)
          _ <- assertHaveRights(p, u)
          p <- Poll.start(p, n)
        } yield (ps.copy(m = ps.m updated(id, p)), Unit)
    }

  def stopPoll(id: Int, u: User, n: LocalDateTime): StateT[Try, Polls, Unit] =
    StateT[Try, Polls, Unit] {
      ps =>
        for {
          _ <- assertPollContains(ps, id)
          p = ps.m(id)
          _ <- assertHaveRights(p, u)
          p <- Poll.stop(p, n)
        } yield (ps.copy(m = ps.m updated(id, p)), Unit)
    }

  def pollResult(id: Int, n: LocalDateTime): StateT[Try, Polls, Poll] =
    StateT[Try, Polls, Poll] {
      ps =>
        for {
          _ <- assertPollContains(ps, id)
          p = ps.m(id)
          _ <- assertIsNotVisible(p, n)
        } yield (ps, p)
    }
}
