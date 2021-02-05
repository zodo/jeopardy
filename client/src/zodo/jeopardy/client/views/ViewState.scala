package zodo.jeopardy.client.views

import zodo.jeopardy.actors.GameActor

case class RootState(isLoading: Boolean, viewState: ViewState) {
  def complete(viewState: ViewState): RootState = copy(isLoading = false, viewState = viewState)
  def loading: RootState = copy(isLoading = true)
}

sealed trait ViewState
object ViewState {
  case object Anonymous extends ViewState
  case class Authorized(name: String, errorMessage: Option[String] = None) extends ViewState
  case class InGame(id: String, packId: String, players: Seq[PlayerInfo], gameStage: GameActor.State.Stage)
      extends ViewState {
    val me: Option[PlayerInfo] = players.find(_.me)
  }

  sealed trait PlayerState
  object PlayerState {
    case object Idle extends PlayerState
    case object ChoosesQuestion extends PlayerState
  }

  case class PlayerInfo(
    id: String,
    name: String,
    score: Int,
    state: PlayerState,
    me: Boolean
  )
}
