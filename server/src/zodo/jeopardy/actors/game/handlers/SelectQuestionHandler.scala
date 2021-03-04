package zodo.jeopardy.actors.game.handlers

import zio._
import zodo.jeopardy.actors.game.State.Stage.RoundStage
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Idle, Question}
import zodo.jeopardy.model.GameCommand.{SelectQuestion, ShowAnswer}
import zodo.jeopardy.model.GameEvent

object SelectQuestionHandler extends Handler[SelectQuestion] {
  override def process(m: SelectQuestion, ctx: HandlerContext) = {
    case state @ RoundStage(i: Idle, round) =>
      val canFail = for {
        question     <- ZIO.fromOption(round.model.themes.flatMap(_.questions).find(_.id == m.questionId))
        _            <- ZIO.fail(()).when(round.takenQuestions.contains(m.questionId))
        _            <- ZIO.fail(()).when(round.activePlayer != m.playerId)
        (qCdId, qCd) <- ctx.setCountdown(ctx.config.hitTheButtonTimeout)(_ ! ShowAnswer(question))
        newStage = round.copy(stage = Question(question, qCdId))
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
        _ <- ctx.stoppedCountdown(i.cdId)
      } yield state
        .withCd(qCdId, qCd)
        .withoutCd(i.cdId)
        .withStage(newStage)

      canFail.orElseSucceed(state)
  }
}
