package zodo.jeopardy.client.views

import zodo.jeopardy.model.PackModel

case class RootState(isLoading: Boolean, viewState: ViewState) {
  def complete(viewState: ViewState): RootState = copy(isLoading = false, viewState = viewState)
  def loading: RootState = copy(isLoading = true)
}

sealed trait ViewState
object ViewState {
  case object Anonymous extends ViewState
  case class Authorized(name: String, errorMessage: Option[String]) extends ViewState
  case class InGame(gameInfo: GameInfo, gameState: GameState) extends ViewState

  sealed trait GameState
  object GameState {
    case object WaitingForStart extends GameState
    case class InRound(round: PackModel.Round, takenQuestions: Set[String]) extends GameState
    case class InQuestion(question: PackModel.Question) extends GameState
    case class InAnswer(answer: PackModel.Answers) extends GameState
  }

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
