import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.scalatest._
import Commands._
import models.Poll

import scala.util.{Failure, Success, Try}


class CommandParserTests extends FlatSpec with Matchers {
  private val parse = (x: String) => CommandParser.parse(x)
  private val timeFrom = (x: String) =>
    LocalDateTime.parse(x, DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))

  "A Parser" should "parse create_poll correctly" in {
    val start = "21:00:59 02:01:30"
    val end = "23:40:59 05:01:29"

    assertResult(parse("/create_poll (name)"))(Success(CreatePoll("name")))
    assertResult(parse("/create_poll ((())) (no)"))(Success(CreatePoll("( )", anonymity = false)))
    assertResult(parse("/create_poll (name) ( yes )   (continuous) " + s"($start)"))(Success(CreatePoll("name",
      anonymity = true,
      visibility = true,
      Option(timeFrom(start)))))
    assertResult(parse("/create_poll(name)(yes)(continuous)" + s"($start)($end)"))(Success(CreatePoll("name",
      anonymity = true,
      visibility = true,
      Option(timeFrom(start)),
      Option(timeFrom(end)))))
  }

}