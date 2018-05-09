package models

object Types {
  type User = Long

  type GeneralState = (Polls, Contexts)

  sealed trait QuestionType
  case object Open extends QuestionType
  case object Choice extends QuestionType
  case object Multi extends QuestionType

  type OpenAnswer = (Option[User], String)
  type ChoiceAnswer = (Option[User], Int)
  type MultiAnswer = (Option[User], Set[Int])
}
