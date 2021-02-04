package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait ParametrizedClientEvent[+A]
sealed trait ClientEvent extends ParametrizedClientEvent[Unit]

object ClientEvent {
  case class EnterGame(name: String, gameId: String) extends ClientEvent
  case class UploadFile(hash: String, pack: PackModel.Pack) extends ClientEvent
  case class ChooseQuestion(questionId: String) extends ClientEvent
  case object PressAnswerButton extends ClientEvent
}
