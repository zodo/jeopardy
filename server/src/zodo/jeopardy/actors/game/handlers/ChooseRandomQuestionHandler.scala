package zodo.jeopardy.actors.game.handlers

import zio._
import zodo.jeopardy.actors.game.State.Stage.RoundStage
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Idle, Question}
import zodo.jeopardy.model.GameCommand.ShowAnswer
import zodo.jeopardy.model.GameEvent

object ChooseRandomQuestionHandler extends Handler[Unit] {
  override def process(m: Unit, ctx: HandlerContext) = {
    case state @ RoundStage(_: Idle, round) =>
      val availableQuestions =
        round.model.themes.flatMap(_.questions).filterNot(q => round.takenQuestions.contains(q.id))
      for {
        randomQuestion <- random.nextIntBounded(availableQuestions.length).map(availableQuestions)
        (qCdId, qCd)   <- ctx.setCountdown(ctx.config.hitTheButtonTimeout)(_ ! ShowAnswer(randomQuestion))
        newStage = round.copy(stage = Question(randomQuestion, qCdId))
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state
        .withCd(qCdId, qCd)
        .withStage(newStage)
  }
}
