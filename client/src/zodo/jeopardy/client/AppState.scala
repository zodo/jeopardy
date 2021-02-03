package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait AppState

object AppState {
  case object Anonymous extends AppState

  case class InGame(gameInfo: GameInfo, gameState: GameState) extends AppState

  sealed trait GameState
  case class InRound(round: PackModel.Round, takenQuestions: Set[String]) extends GameState
  case class InQuestion(question: PackModel.Question) extends GameState
  case class InAnswer(answer: PackModel.Answers) extends GameState

  case class GameInfo(
    id: String,
    packHash: String,
    players: Seq[PlayerInfo]
  ) {
    def me: PlayerInfo = players.filter(_.me).head
  }

  case class PlayerInfo(
    id: String,
    name: String,
    score: String,
    givingAnswer: Boolean,
    me: Boolean
  )
}
