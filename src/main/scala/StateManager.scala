import java.time.LocalDateTime

import Commands._
import Models.{Poll, Polls}
import Results._
import scalaz.State


object StateManager {
  val performAction: Command => State[Polls, CommandResult] = {
    case CreatePoll(name, anonymity, visibility, startTime, stopTime) =>
      for {
        id <- Polls.addPoll(Poll(name, anonymity, visibility, startTime, stopTime))
      } yield CreatePollResult(id)

    case List_() => for {
      polls <- State.get[Polls]
    } yield ListResult(polls.map.values.toList)

    case DeletePoll(id) => for {
      id <- Polls.deletePoll(id)
    } yield DeletePollResult(id.getOrElse(-1))

    case StartPoll(id) => for {
      id <- Polls.startPoll(id)
    } yield StartPollResult(id.getOrElse(-1))
    //State.modify[Polls]
    /*State { (s: Polls) =>
      val poll = s.map.get(id)
      val now = LocalDateTime.now()
      poll match {
        case Some(v) =>
          if (Poll.couldStart(v, now))
            (s.copy(map = s.map.updated(id, v.copy(startTime = Option{now}))), StartPollResult(id))
          else
            (s, StartPollResult(-1))
        case None =>
          (s, CreatePollResult(-1))
      }
    }*/

    case StopPoll(id) => for {
      id <- Polls.stopPoll(id)
    } yield StopPollResult(id.getOrElse(-1))
    /*State { (s: Polls) =>
      val poll = s.map.get(id)
      val now = LocalDateTime.now()
      poll match {
        case Some(v) =>
          if (Poll.couldStop(v, now))
            (s.copy(map = s.map.updated(id, v.copy(stopTime = Option{now}))), StopPollResult(id))
          else
            (s, StopPollResult(-1))
        case None =>
          (s, StopPollResult(-1))
      }
    }*/

    case Unrecognized => State.state[Polls, CommandResult](Bad)
  }
}
