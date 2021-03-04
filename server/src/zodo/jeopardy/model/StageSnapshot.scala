package zodo.jeopardy.model

sealed trait StageSnapshot
object StageSnapshot {
  case object BeforeStart extends StageSnapshot
  case class Round(model: PackModel.Round, takenQuestions: Set[String], activePlayer: String) extends StageSnapshot
  case class Question(model: PackModel.Question) extends StageSnapshot
  case class AnswerAttempt(model: PackModel.Question, activePlayer: String) extends StageSnapshot
  case class Answer(model: PackModel.Answers) extends StageSnapshot
}
