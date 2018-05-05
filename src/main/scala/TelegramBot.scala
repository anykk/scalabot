import scala.io.Source
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._
import models.Polls

import scala.util.{Success, Failure}



object TelegramBot extends TelegramBot with Polling {
  lazy val token = Source.fromFile("src\\main\\scala\\bot.token").getLines().mkString // safe token
  val parser = CommandParser
  var ps = Polls(Map.empty)
  val manager = StateManager


  override def receiveMessage(msg: Message): Unit = {
    for (text <- msg.text)
      request(SendMessage(msg.source, {
        ???
      }
      ))
  }

}