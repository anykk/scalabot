package models

import Types.{ChoiceAnswer, MultiAnswer, OpenAnswer, User}

sealed trait Question {
  val name: String
  val options: List[String]
  val answered: List[User]
}

final case class OpenQuestion(name: String,
                        answers: List[OpenAnswer] = Nil,
                        answered: List[User] = Nil,
                        options: List[String] = Nil
                       ) extends Question

final case class ChoiceQuestion(name: String,
                          options: List[String],
                          answers: List[ChoiceAnswer] = Nil,
                          answered: List[User] = Nil
                         ) extends Question

final case class MultiQuestion(name: String,
                         options: List[String],
                         answered: List[User] = Nil,
                         answers: List[MultiAnswer] = Nil
                        ) extends Question

object Question {

}