package zodo.jeopardy.client.views

import zodo.jeopardy.model.StageSnapshot.BeforeStart
import zodo.jeopardy.model.{PlayerId, StageSnapshot}

sealed trait ViewState
object ViewState {
  case object Anonymous extends ViewState
  case class Authorized(name: String, errorMessage: Option[String] = None) extends ViewState
  case class ErrorMessage(msg: String) extends ViewState
  case class RedirectToGame(gameId: String) extends ViewState
  case class InGame(
    id: String,
    packId: String,
    players: Seq[PlayerInfo] = Nil,
    playerEvents: PlayerEvents = PlayerEvents(Map()),
    stage: StageSnapshot = BeforeStart,
    countdown: Option[Countdown] = None
  ) extends ViewState {
    val me: Option[PlayerInfo] = players.find(_.me)
    def withPlayerEvent(id: PlayerId, map: PlayerEvent => PlayerEvent): InGame = {
      copy(playerEvents = playerEvents.withPlayerEvent(id, map))
    }
  }

  sealed trait PlayerState
  object PlayerState {
    case object Idle extends PlayerState
    case object ChoosesQuestion extends PlayerState
    case object ThinkingAboutAnswer extends PlayerState
  }

  case class PlayerInfo(
    id: PlayerId,
    name: String,
    score: Int,
    state: PlayerState,
    me: Boolean,
    disconnected: Boolean = false
  )

  case class PlayerEvents(
    events: Map[PlayerId, PlayerEvent]
  ) {
    def withPlayerEvent(id: PlayerId, map: PlayerEvent => PlayerEvent): PlayerEvents = {
      copy(events = events.updatedWith(id) {
        case None     => Some(map(PlayerEvent()))
        case Some(pe) => Some(map(pe))
      })
    }

    def isButtonPressed(id: PlayerId) = events.get(id).fold(false)(_.buttonPressed)

    def guess(id: PlayerId) = events.get(id).flatMap(_.guess)
  }

  case class PlayerEvent(
    buttonPressed: Boolean = false,
    guess: Option[PlayerGuess] = None
  )

  case class PlayerGuess(answer: String, isCorrect: Boolean)

  case class Countdown(value: Int, max: Int) {
    val remaining = max - value
  }
}
