package zodo.jeopardy.actors.game.handlers

import zio._
import zodo.jeopardy.actors.game.State.Stage.{Round, RoundStage}
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Idle, Question, ReadyForHit}
import zodo.jeopardy.model.GameCommand.FinishQuestion
import zodo.jeopardy.model.{GameCommand, GameEvent}

object FinishQuestionHandler extends Handler[FinishQuestion] {
  override def process(m: FinishQuestion, ctx: HandlerContext) = {
    case state @ RoundStage(q: Question, r: Round) =>
      val readyPlayers =
        if (m.questionId == q.model.id)
          q.readyPlayers.updatedWith(m.playerId)(
            _.map(fragmentsCount => if (m.force) q.model.fragments.size else fragmentsCount + 1)
          )
        else
          q.readyPlayers

      if (readyPlayers.values.forall(_ >= q.model.fragments.size)) {
        for {
          (hitCdId, qCd) <- ctx.setCountdown(ctx.config.hitTheButtonTimeout)(_ ! GameCommand.ShowAnswer(q.model))
          newStage = r.copy(stage = ReadyForHit(q.model, hitCdId))
          _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
        } yield state
          .withCd(hitCdId, qCd)
          .withStage(newStage)
      } else {
        UIO(state.withStage(r.copy(stage = q.copy(readyPlayers = readyPlayers))))
      }
  }
}
