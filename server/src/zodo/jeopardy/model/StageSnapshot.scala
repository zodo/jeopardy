package zodo.jeopardy.model

sealed trait StageSnapshot
object StageSnapshot {
  case object BeforeStart extends StageSnapshot
  case class Round(round: PackModel.Round, takenQuestions: Set[String], activePlayer: String) extends StageSnapshot
  case class Question(question: PackModel.Question) extends StageSnapshot
  case class AnswerAttempt(question: PackModel.Question, activePlayer: String) extends StageSnapshot
  case class Answer(answer: PackModel.Answers) extends StageSnapshot
}
