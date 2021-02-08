package zodo.jeopardy.model

import zodo.jeopardy.actors.GameActor.CountdownId
import zodo.jeopardy.actors.PlayerActorRef

sealed trait GameCommand[+_]
object GameCommand {
  case class AddPlayer(id: String, name: String, reply: PlayerActorRef) extends GameCommand[Unit]
  case object Start extends GameCommand[Unit]
  case class SelectQuestion(playerId: String, questionId: String) extends GameCommand[Unit]
  case class HitButton(playerId: String) extends GameCommand[Unit]
  case class GiveAnswer(playerId: String, answer: String) extends GameCommand[Unit]
  case object ReturnToRound extends GameCommand[Unit]
  case class ShowAnswer(question: PackModel.Question) extends GameCommand[Unit]
  case class TickCountdown(tick: Int, id: CountdownId) extends GameCommand[Unit]
  case object ChooseRandomQuestion extends GameCommand[Unit]
  case object PlayerDontKnowAnswer extends GameCommand[Unit]
}
