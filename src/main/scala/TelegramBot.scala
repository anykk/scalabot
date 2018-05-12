import java.time.LocalDateTime

import scala.io.Source
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
import models.{Contexts, Polls}

import scala.util.{Failure, Success}


object TelegramBot extends TelegramBot with Polling {
  lazy val token = Source.fromFile("src\\main\\scala\\bot.token").getLines().mkString // safe token

  var state = (Polls(Map.empty), Contexts(Map.empty))

  val parse = (x: String) => CommandParser.parse(x)

  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text)
      request(SendMessage(msg.source,
        parse(text) match {
          case Success(cmd) =>
            val ms = StateManager(state, msg.source, cmd, LocalDateTime.now)
            ms match {
              case Success(sa) =>
                state = sa._1
                Render(sa._2)
              case Failure(e) =>
                e.getMessage
            }
          case Failure(e) =>
            e.getMessage
      }
      ))
  }

}