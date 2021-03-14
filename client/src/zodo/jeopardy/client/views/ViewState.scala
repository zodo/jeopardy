package zodo.jeopardy.client.views

import zodo.jeopardy.model.StageSnapshot.{AnswerAttempt, BeforeStart, Round}
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
    val me = {
      players.find(_.me) match {
        case Some(player) =>
          Me(
            canHitButton = stage match {
              case r: Round => !r.previousAnswers.contains(player.id)
              case _        => true
            },
            canAnswer = stage match {
              case a: AnswerAttempt => a.activePlayer == player.id
              case _                => false
            },
            canAppeal = stage match {
              case r: Round => r.previousAnswers.get(player.id).contains(false) && !r.triedToAppeal.contains(player.id)
              case _        => false
            },
            canVoteAppeal = !playerEvents.appeal(player.id).contains(AppealInitiated)
          )
        case None => Me()
      }
    }

    val appealPlayerName = playerEvents.events
      .find(_._2.appeal.contains(AppealInitiated))
      .map(_._1)
      .flatMap(playerId => players.find(_.id == playerId))
      .map(_.name)

    def withPlayerEvent(id: PlayerId, map: PlayerEvent => PlayerEvent): InGame = {
      copy(playerEvents = playerEvents.withPlayerEvent(id, map))
    }
    def withPlayerEvent(map: (PlayerId, PlayerEvent) => PlayerEvent): InGame = {
      copy(playerEvents = PlayerEvents(playerEvents.events.map {
        case (key, value) => key -> map(key, value)
      }))
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

    def appeal(id: PlayerId) = events.get(id).flatMap(_.appeal)
  }

  case class PlayerEvent(
    buttonPressed: Boolean = false,
    guess: Option[PlayerGuess] = None,
    appeal: Option[AppealState] = None
  )

  case class PlayerGuess(answer: String, isCorrect: Boolean)

  sealed trait AppealState
  case object AppealNotChosen extends AppealState
  case object AppealAgree extends AppealState
  case object AppealDisagree extends AppealState
  case object AppealInitiated extends AppealState

  case class Countdown(value: Int, max: Int) {
    val remaining = max - value
  }

  case class Me(
    canHitButton: Boolean = false,
    canAnswer: Boolean = false,
    canAppeal: Boolean = false,
    canVoteAppeal: Boolean = false
  )
}
