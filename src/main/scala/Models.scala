import java.time.LocalDateTime


package object Models {
  private val idStream = Stream.from(1).iterator

  case class Poll(name: String,
                  //owner: User, TODO realizzzeIT
                  anonymity: Boolean,
                  visibility: Boolean,
                  startTime: Option[LocalDateTime],
                  stopTime: Option[LocalDateTime]) {

    override def toString: String = {
      s"""Name: $name
         |Anonymity: $anonymity
         |Visibility: $visibility
         |Start time: ${startTime.getOrElse("not set")}
         |Stop time: ${stopTime.getOrElse("not set")}
     """.stripMargin
    }
  }

  object Poll {
    def started(poll: Poll, now: LocalDateTime): Boolean =
      poll.startTime.exists(now.isAfter)

    def stopped(poll: Poll, now: LocalDateTime): Boolean =
      poll.stopTime.exists(now.isAfter)

    def active(poll: Poll, now: LocalDateTime): Boolean =
      started(poll, now) && !stopped(poll, now)

    def visible(poll: Poll, now: LocalDateTime): Boolean =
      poll.visibility && started(poll, now) || !poll.visibility && stopped(poll, now)
  }

  case class State(polls: Map[Int, Poll])

  object State {
    def addPoll(state: State, poll: Poll): State = {
      val id = idStream.next()
      state.copy(polls = state.polls + (id -> poll))
    }

    def deletePoll(state: State, id: Int, now: LocalDateTime): State = {
      ???
    }
  }
}
