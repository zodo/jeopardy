package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait AppState

object AppState {
  case object BeforeStart extends AppState
  case class ShowInfo(hash: String, pack: PackModel.Pack) extends AppState
  case class QuestionDetails(hash: String, prev: AppState, question: PackModel.Question) extends AppState
  case class Error(message: String) extends AppState
}
