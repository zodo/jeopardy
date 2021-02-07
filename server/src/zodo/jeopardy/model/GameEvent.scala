package zodo.jeopardy.model

sealed trait GameEvent[+_]
object GameEvent {
  case class PlayerAdded(player: Player) extends GameEvent[Unit]
  case class PlayerScoreUpdated(playerId: String, scoreDiff: Int) extends GameEvent[Unit]
  case class PlayerHitTheButton(playerId: String) extends GameEvent[Unit]
  case class StageUpdated(stage: StageSnapshot) extends GameEvent[Unit]
  case class PlayerGaveAnswer(playerId: String, answer: String, isCorrect: Boolean) extends GameEvent[Unit]
  case class CountdownUpdated(v: Option[Countdown]) extends GameEvent[Unit]

  case class Player(
    id: String,
    name: String
  )

  case class Countdown(value: Int, max: Int)
}
