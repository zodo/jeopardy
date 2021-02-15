package zodo.jeopardy.model

sealed trait GameEvent[+_]
object GameEvent {
  case class PlayersUpdated(players: Seq[Player]) extends GameEvent[Unit]
  case class PlayerHitTheButton(playerId: String) extends GameEvent[Unit]
  case class StageUpdated(stage: StageSnapshot) extends GameEvent[Unit]
  case class PlayerGaveAnswer(playerId: String, answer: String, isCorrect: Boolean) extends GameEvent[Unit]
  case class CountdownUpdated(v: Option[CountdownModel]) extends GameEvent[Unit]

  case class Player(
    id: String,
    name: String,
    score: Int,
    disconnected: Boolean
  )

  case class CountdownModel(value: Int, max: Int)
}
