import java.time.LocalDateTime

import models.{Contexts, Poll, Polls}

import scala.util.{Failure, Success}

object Main {

  val parse = (s: String) => CommandParser.parse(s)
  var state = (Polls(Map.empty), Contexts(Map.empty))

  def main(args: Array[String]): Unit = {
    loop()
  }

  @scala.annotation.tailrec
  def loop(): Unit = {
    parse(scala.io.StdIn.readLine("Command: ")) match {
      case Success(v) =>
        val ts = StateManager(state, 1, v, LocalDateTime.now)
        ts match {
          case Success(va) =>
            state = va._1
            println(va._2.toString)
          case Failure(e) => println(e.getMessage)
        }
      case Failure(e) => println(e.getMessage)
    }
    loop()
  }
}

