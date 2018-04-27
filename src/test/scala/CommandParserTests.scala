import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import org.scalatest._

import CommandParser._
import Commands._


class CommandParserTests extends  FlatSpec with Matchers {

  it should "parse CreatePoll" in {

    parse("/create_poll (((how are you, darling?))") should be
    CreatePoll("(how are you darling?)", false, false, None, None)

    parse("/create_poll (((how are you, darling?)))  (yes)") should be
    CreatePoll("(how are you, darling?)", true, false, None, None)

    parse("/create_poll (((how are you, darling?)))  (no)") should be
    CreatePoll("(how are you, darling?)", false, false, None, None)

    parse("/create_poll (((how are you, darling?)))  (yes) (continuous)") should be
    CreatePoll("(how are you, darling?)", true, true, None, None)

    parse("/create_poll (((how are you, darling?)))  (yes) (continuous) (22:22:22 2018:04:20)") should be
    CreatePoll("(how are you, darling?)", true, true,
      Option(LocalDateTime.parse("22:22:22 18:04:20", DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))), None)

    parse("/create_poll (((how are you, darling?)))  (yes) (continuous) (22:22:22 2018:04:20) (22:22:22 2018:04:21)") should be
    CreatePoll("(how are you, darling?)", true, true,
      Option(LocalDateTime.parse("22:22:22 18:04:20", DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))),
      Option(LocalDateTime.parse("22:22:22 18:04:21", DateTimeFormatter.ofPattern("HH:mm:ss yy:MM:dd"))))

    parse("/create_poll ((illegalcommand") should be
    UnknownCommand("/create_poll ((illegalcommand")
  }

  it should "parse List " in {

    parse("/list illegalcommand") should be
    UnknownCommand("/list illegalcommand")

    parse("/list") should be
    List()

  }

  it should "parse DeletePoll" in{

    parse("/delete_poll") should be
    UnknownCommand("/delete_poll")

    parse("/delete_poll 1") should be
    DeletePoll(1)
  }

  it should "parse StartPoll" in {

    parse("/start_poll") should be
    UnknownCommand("/start_poll")

    parse("/start_poll 1") should be
    StartPoll(1)
  }

  it should "parse StopPoll" in {

    parse("/stop_poll") should be
    UnknownCommand("/stop_poll")

    parse("/stop_poll 1") should be
    StopPoll(1)
  }

}