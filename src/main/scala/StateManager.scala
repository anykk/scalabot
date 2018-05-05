import Commands._
import Results._
import cats.data.StateT
import models.{Contexts, Poll, Polls}
import models.Types.User

import scala.util.Try

object StateManager {
  //type GenState = (Polls, Contexts) [?]

  import cats.implicits._

  //apply(state: (Polls, Contexts), cmd: Command): Try[(Polls, Contexts), CommandResult]
  def performAction(user: User, triedCommand: Try[Command]): StateT[Try, Polls, CommandResult] =
    {
      ??? //TODO: How to generalize StateT?
    }

  def createPoll(u: User, r: CreatePoll): StateT[Try, Polls, PollCreated] =
    for {
      id <- Polls.addPoll(Poll(r.name, r.anonymity, r.visibility,
        r.startTime, r.stopTime, u, Vector.empty))
    } yield PollCreated(id)

  def list(r: List_): StateT[Try, Polls, ListResult] =
    for {
      s <- StateT.get[Try, Polls]
    } yield ListResult(s.m.values.toList)

  def deletePoll(r: DeletePoll): StateT[Try, Polls, PollDeleted] =
    for {
      id <- Polls.deletePoll(r.id)
    } yield PollDeleted(id)

  def startPoll(u: User, r: StartPoll): StateT[Try, Polls, PollStarted] =
    for {
      id <- Polls.startPoll(r.id, u)
    } yield PollStarted(id)

  def stopPoll(u: User, r: StopPoll): StateT[Try, Polls, PollStopped] =
    for {
      id <- Polls.stopPoll(r.id, u)
    } yield PollStopped(id)

  def result(r: Result): StateT[Try, Polls, PollingResult] =
    for {
      p <- Polls.pollResult(r.id)
    } yield PollingResult(p)

  def begin(u: User, r: Begin): StateT[Try, Contexts, BeginResult] = ???

  def end(u: User, r: End): StateT[Try, Contexts, EndResult] = ???

  def view(u: User, r: View): StateT[Try, (Polls, Contexts), ViewResult] = ???

  def addQuestion(u: User, r: AddQuestion): StateT[Try, (Polls, Contexts), QuestionAdded] = ???

  def deleteQuestion(u: User, r: DeleteQuestion): StateT[Try, (Polls, Contexts), QuestionDeleted] = ???

  def answer(u: User, r: Answer): StateT[Try, (Polls, Contexts), AnswerResult] = ???
}


