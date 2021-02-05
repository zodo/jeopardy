package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait AppState

object AppState {
  case object Anonymous extends AppState

  case class Authorized(name: String, errorMessage: Option[String]) extends AppState

  case class InGame(gameInfo: GameInfo, gameState: GameState) extends AppState

  sealed trait GameState
  case object WaitingForStart extends GameState
  case class InRound(round: PackModel.Round, takenQuestions: Set[String]) extends GameState
  case class InQuestion(question: PackModel.Question) extends GameState
  case class InAnswer(answer: PackModel.Answers) extends GameState

  case class GameInfo(
    id: String,
    packHash: String,
    players: Seq[PlayerInfo]
  ) {
    def me: Option[PlayerInfo] = players.find(_.me)
  }

  sealed trait PlayerState

  object PlayerState {
    case object Idle extends PlayerState
    case object Choose extends PlayerState
    case object Answer extends PlayerState
  }

  case class PlayerInfo(
    id: String,
    name: String,
    score: Int,
    state: PlayerState,
    me: Boolean
  )
}
