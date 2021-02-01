package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait AppState

object AppState {
  case object BeforeStart extends AppState
  case class ShowInfo(pack: PackModel.Pack) extends AppState
  case class QuestionDetails(prev: AppState, question: PackModel.Question) extends AppState
  case class Error(message: String) extends AppState
}
