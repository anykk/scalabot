import java.time.LocalDateTime

import Commands._
import Results._
import cats.data.StateT
import models.{Contexts, Poll, Polls}
import models.Types.{GeneralState, User}

import scala.util.{Success, Try}

object StateManager {
  import cats.implicits.catsStdInstancesForTry

  def apply(o_s: (Polls, Contexts),
            u: User, cmd: Command,
            n: LocalDateTime): Try[(GeneralState, CommandResult)] =
  {
    def withPolls[A <: CommandResult](a: StateT[Try, Polls, A]): Try[(GeneralState, A)] =
      for {
        s <- a.run(o_s._1)
      } yield ( (s._1, o_s._2.copy()), s._2 )

    def withContexts[A <: CommandResult](a: StateT[Try, Contexts, A]): Try[(GeneralState, A)] =
      for {
        s <- a.run(o_s._2)
      } yield ( (o_s._1.copy(), s._1), s._2 )

    def both[A <: CommandResult](a: StateT[Try, GeneralState, A]): Try[(GeneralState, A)] =
      for {
        s <- a.run(o_s)
      } yield ( s._1, s._2 )

    cmd match {
      case r: CreatePoll =>
        withPolls(createPoll(u, r))

      case r: List_ =>
        withPolls(list(r))

      case r: DeletePoll =>
        withPolls(deletePoll(r, n))

      case r: StartPoll =>
        withPolls(startPoll(u, r, n))

      case r: StopPoll =>
        withPolls(stopPoll(u, r, n))

      case r: Result =>
        withPolls(result(r, n))

      case r: Begin =>
        both(begin(u, r))

      case r: End =>
        withContexts(end(u))

      case r: View =>
        both(view(u))
    }
  }

  def createPoll(u: User, r: CreatePoll): StateT[Try, Polls, PollCreated] =
    for {
      id <- Polls.addPoll(Poll(r.name, r.anonymity, r.visibility,
        r.startTime, r.stopTime, u, Vector.empty))
    } yield PollCreated(id)

  def list(r: List_): StateT[Try, Polls, ListResult] =
    for {
      s <- StateT.get[Try, Polls]
    } yield ListResult(s.m.toList)

  def deletePoll(r: DeletePoll, n: LocalDateTime): StateT[Try, Polls, PollDeleted] =
    for {
      _ <- Polls.deletePoll(r.id, n)
    } yield PollDeleted()

  def startPoll(u: User, r: StartPoll, n: LocalDateTime): StateT[Try, Polls, PollStarted] =
    for {
      _ <- Polls.startPoll(r.id, u, n)
    } yield PollStarted()

  def stopPoll(u: User, r: StopPoll, n: LocalDateTime): StateT[Try, Polls, PollStopped] =
    for {
      _ <- Polls.stopPoll(r.id, u, n)
    } yield PollStopped()

  def result(r: Result, n: LocalDateTime): StateT[Try, Polls, PollingResult] =
    for {
      p <- Polls.pollResult(r.id, n)
    } yield PollingResult(p)

  def begin(u: User, r: Begin): StateT[Try, GeneralState, BeginResult] =
    for {
      _ <- Contexts.begin(u, r.id)
    } yield BeginResult()

  def end(u: User): StateT[Try, Contexts, EndResult] =
    for {
      _ <- Contexts.end(u)
    } yield EndResult()

  def view(u: User): StateT[Try, GeneralState, ViewResult] =
    for {
      p <- Contexts.view(u)
    } yield ViewResult(p)

  def addQuestion(u: User, r: AddQuestion): StateT[Try, GeneralState, QuestionAdded] = ???

  def deleteQuestion(u: User, r: DeleteQuestion): StateT[Try, GeneralState, QuestionDeleted] = ???

  def answer(u: User, r: Answer): StateT[Try, GeneralState, AnswerResult] = ???
}


