import java.time.LocalDateTime

import Requests._
import StateModels._


object PollManager {
  private val idStream = Stream.from(1).iterator
  var polls = State(Map.empty)

  def execute(polls: State, command: Request): (State, String) = {
    (command: @unchecked) match {
      case CreatePollRequest(name, anonymity, visibility, startTime, stopTime) =>
        val id = idStream.next()
        polls.copy(polls = polls.polls +
          (id -> Poll(name, anonymity,
            visibility, startTime, stopTime))) ->
          s"Poll was successfully created with $id id"

      case ListRequest() =>
        polls.copy() -> polls.polls.mkString("\n")

      case DeletePollRequest(id) =>
        polls.polls.get(id) match {
          case Some(poll) =>
            if (!Poll.active(poll, LocalDateTime.now()))
              polls.copy(polls = polls.polls - id) ->
                s"Poll with $id id was successfully deleted"
            else
              polls.copy() -> s"Poll with $id is active"
          case None =>
            polls.copy() -> s"Poll with $id id doesn't exist"
        }

      case StartPollRequest(id) =>
        polls.polls.get(id) match {
          case Some(poll) =>
            if (poll.stopTime.isEmpty)
              polls.copy(polls = polls.polls +
                (id -> poll.copy(startTime = Option(LocalDateTime.now())))) ->
                s"Poll with $id id was successfully started"
            else
              polls.copy() -> s"Can't start poll with $id id"
          case None =>
            polls.copy() -> s"Poll with $id doesn't exist"
        }

      case StopPollRequest(id) =>
        polls.polls.get(id) match {
          case Some(poll) =>
            if (poll.stopTime.isEmpty)
              polls.copy(polls = polls.polls +
                (id -> poll.copy(stopTime = Option(LocalDateTime.now())))) ->
                s"Poll with $id id was successfully stopped"
            else
              polls.copy() -> s"Can't stop poll with $id id"
          case None =>
            polls.copy() -> s"Poll with $id doesn't exist"
        }

      case IllegalRequest(message) =>
        polls.copy() -> message
      }
    //case command: Result => TODO
  }

  def applyCommand(polls: State, command: Request): String = {
    val result = execute(polls, command)
    this.polls = result._1
    result._2
  }
}
