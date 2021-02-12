package zodo.jeopardy.client.views

import zodo.jeopardy.model.StageSnapshot

sealed trait ViewState
object ViewState {
  case object Anonymous extends ViewState
  case class Authorized(name: String, errorMessage: Option[String] = None) extends ViewState
  case class ErrorMessage(msg: String) extends ViewState
  case class InGame(
    id: String,
    packId: String,
    players: Seq[PlayerInfo],
    stage: StageSnapshot,
    countdown: Option[Countdown]
  ) extends ViewState {
    val me: Option[PlayerInfo] = players.find(_.me)
    def withPlayers(mapOnly: PlayerInfo => Boolean, map: PlayerInfo => PlayerInfo): InGame = {
      copy(players = players.map(p => if (mapOnly(p)) map(p) else p))
    }
  }

  sealed trait PlayerState
  object PlayerState {
    case object Idle extends PlayerState
    case object ChoosesQuestion extends PlayerState
    case object ThinkingAboutAnswer extends PlayerState
  }

  case class PlayerInfo(
    id: String,
    name: String,
    score: Int,
    state: PlayerState,
    me: Boolean,
    buttonPressed: Boolean = false,
    guess: Option[PlayerGuess] = None,
    disconnected: Boolean = false
  )

  case class PlayerGuess(answer: String, isCorrect: Boolean)

  case class Countdown(value: Int, max: Int) {
    val remaining = max - value
  }
}
