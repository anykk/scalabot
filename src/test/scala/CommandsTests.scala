import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import Commands._
import Results._
import models.{Contexts, Poll, Polls, Types}
import org.scalatest._

import scala.util.{Failure, Success, Try}


class CommandsTests extends FlatSpec with Matchers {

  val testmap = Map(1 -> Poll("name", true, false, None, None, 1, Vector.empty ),
                           2 -> Poll("name2", true, false, None, None, 1, Vector.empty ))

  "State Manager" should "create poll" in {
    assertResult(StateManager((Polls(Map.empty), Contexts(Map.empty)), 1, CreatePoll("name"), LocalDateTime.now))(Try {
      ((Polls(Map(1 -> Poll("name", true, false, None, None, 1, Vector.empty )), 2), Contexts(Map.empty)), PollCreated(1)) })
  }

  it should "delete poll " in {
    assertResult(StateManager((Polls(testmap), Contexts(Map.empty)), 1, DeletePoll(1), LocalDateTime.now)) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, None, None, 1, Vector.empty)), 1) , Contexts(Map.empty)) , PollDeleted()) })

    assert(StateManager((Polls(testmap), Contexts(Map.empty)), 1, DeletePoll(3), LocalDateTime.now).isFailure)
  }

  private val timeFrom = (x: String) =>
    LocalDateTime.parse(x, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))

  val start = "21:00:59 02:01:30"
  val end = "23:40:59 05:01:29"

  it should "stop poll" in {
    assertResult(StateManager((Polls(Map(2 -> Poll("name2", true, false, Option(timeFrom(start)),
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StopPoll(2), timeFrom(end))) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, Option(timeFrom(start)),
        Option(timeFrom(end)) , 1, Vector.empty)), 1) , Contexts(Map.empty)) , PollStopped()) })

    assert(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StopPoll(2), LocalDateTime.now).isFailure)

    assert(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StopPoll(3), LocalDateTime.now).isFailure)

    assert(StateManager((Polls(Map(2 -> Poll("name2", true, false, Option(timeFrom(start)),
      Option(timeFrom(end)), 1, Vector.empty))), Contexts(Map.empty)), 1, StopPoll(2), LocalDateTime.now).isFailure)
  }

  it should "start poll" in {
    assertResult(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StartPoll(2), timeFrom(start))) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, Option(timeFrom(start)),
        None, 1, Vector.empty)), 1) , Contexts(Map.empty)) , PollStarted()) })

    assert(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StartPoll(3), LocalDateTime.now).isFailure)

    assert(StateManager((Polls(Map(2 -> Poll("name2", true, false, Option(timeFrom(start)),
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, StartPoll(2), LocalDateTime.now).isFailure)

  }

  it should "list poll" in {
    assertResult(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), 1, List_(), timeFrom(start))) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, None, None, 1, Vector.empty)), 1) , Contexts(Map.empty)),
        ListResult(List((2,Poll("name2", true, false, None, None, 1, Vector.empty))))) })
  }

  it should "begin" in {
    val user = 1L
    assertResult(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), user, Begin(2), timeFrom(start))) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, None, None, 1, Vector.empty)), 1) , Contexts(Map(user -> 2))),
        BeginResult()) })
  }

  it should "end" in {
    val user = 1L
    assertResult(StateManager((Polls(Map(2 -> Poll("name2", true, false, None,
      None, 1, Vector.empty))), Contexts(Map.empty)), user, Begin(2), timeFrom(start))) (Try{
      ( (Polls(Map(2 -> Poll("name2", true, false, None, None, 1, Vector.empty)), 1) , Contexts(Map(user -> 2))),
        BeginResult()) })
  }

  it should "add_question" in {
    var o_s = (Polls(Map.empty), Contexts(Map.empty))
    assert(StateManager(o_s, 1, AddQuestion("name", Nil), LocalDateTime.now).isFailure)
    o_s = StateManager(o_s, 1, CreatePoll("name"), LocalDateTime.now).flatMap(s =>
      StateManager(s._1, 1, Begin(1), LocalDateTime.now)).get._1
    assertResult(StateManager(o_s, 1, AddQuestion("name", Nil), LocalDateTime.now).get._2)(QuestionAdded(0))
  }

}
