import java.time.LocalDateTime

import models.{Poll, Polls}

import scala.util.{Failure, Success, Try}

object Main {
  var ps = Polls(Map.empty)

  def main(args: Array[String]): Unit = {
    //loop()
    //TelegramBot.run()
  }

  private def loop(): Unit = {
    import cats.implicits._

    //val mr = StateManager.performAction(321561, scala.io.StdIn.readLine("Command: ")).run(ps)
    //mr match {
    //case Success(v) =>
    //ps = v._1
    //println(v._2.toString)
    //case Failure(e) =>
    //  println(e.getMessage)
    ???
  }
}

