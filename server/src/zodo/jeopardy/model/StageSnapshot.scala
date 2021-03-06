package zodo.jeopardy.model

sealed trait StageSnapshot
object StageSnapshot {
  case object BeforeStart extends StageSnapshot
  case class Round(
    model: PackModel.Round,
    takenQuestions: Set[String],
    activePlayer: String,
    previousAnswers: Map[PlayerId, Boolean],
    triedToAppeal: Set[PlayerId]
  ) extends StageSnapshot
  case class Question(model: PackModel.Question) extends StageSnapshot
  case class ReadyForHit(model: PackModel.Question) extends StageSnapshot
  case class AnswerAttempt(model: PackModel.Question, activePlayer: String) extends StageSnapshot
  case class Answer(model: PackModel.Answers) extends StageSnapshot
  case class Appeal(
    model: PackModel.Question,
    answer: String,
    playerId: PlayerId,
    agree: Set[PlayerId],
    disagree: Set[PlayerId]
  ) extends StageSnapshot
  case class AppealResult(resolution: Boolean) extends StageSnapshot
}
