package zodo.jeopardy.client.events

import zodo.jeopardy.model.PackModel

sealed trait ParametrizedClientEvent[+A]
sealed trait ClientEvent extends ParametrizedClientEvent[Unit]

object ClientEvent {
  case class Introduce(name: String) extends ClientEvent
  case object Leave extends ClientEvent
  case class EnterGame(gameId: String) extends ClientEvent
  case object StartGame extends ClientEvent
  case class UploadFile(hash: String, pack: PackModel.Pack) extends ClientEvent
  case class SelectQuestion(questionId: String) extends ClientEvent
  case object HitButton extends ClientEvent
  case class GiveAnswer(value: String) extends ClientEvent
  case class FinishQuestionReading(questionId: String) extends ClientEvent
}
