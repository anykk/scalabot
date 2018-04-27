import java.time.LocalDateTime

import Commands._
import Models.{Poll, Polls}
import Results._
import scalaz.State

object Worker {
  type CommonState = State[Polls, CommandResult]

  def performCommand: Command => CommonState = {
    case CreatePoll(name, anonymity, visibility, startTime, stopTime) => State { (s: Polls) =>
      val (ns, a) = Polls.addPoll(Poll(name, -1, anonymity, visibility, startTime, stopTime, Nil))(s)
      (ns, CreatePollResult(a)) }

    case List_() => State { (s: Polls) =>
      (s, ListResult(s.map.values.toList))
    }

    case DeletePoll(id) => State { (s: Polls) =>
      val (ns, a) = Polls.deletePoll(id)(s)
      a match {
        case -1 => (s, DeletePollResult(-1))
        case id: Int => (ns, DeletePollResult(id))
      }
    }

    case StartPoll(id) => State { (s: Polls) =>
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
    }

    case StopPoll(id) => State { (s: Polls) =>
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
    }

    case Unrecognized => State { (s: Polls) =>
      (s, Bad)
    }
  }
}
