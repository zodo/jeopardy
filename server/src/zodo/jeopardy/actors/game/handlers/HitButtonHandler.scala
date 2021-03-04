package zodo.jeopardy.actors.game.handlers

import zodo.jeopardy.actors.game.State.Stage.RoundStage
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{AwaitingAnswer, Question}
import zodo.jeopardy.model.GameCommand.HitButton
import zodo.jeopardy.model.{GameCommand, GameEvent}

object HitButtonHandler extends Handler[HitButton] {
  override def process(m: HitButton, ctx: HandlerContext) = {
    case state @ RoundStage(q: Question, r) =>
      for {
        maybeCd          <- ctx.stoppedCountdown(q.cdId)
        _                <- ctx.broadcast(GameEvent.PlayerHitTheButton(m.playerId))
        (answerCdId, cd) <- ctx.setCountdown(ctx.config.answerTimeout)(_ ! GameCommand.GiveAnswer(m.playerId, "🤷"))
        newStage = r.copy(stage = AwaitingAnswer(q, m.playerId, answerCdId, maybeCd.fold(1)(_.value)))
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state
        .withCd(answerCdId, cd)
        .withoutCd(q.cdId)
        .withStage(newStage)

    case state @ RoundStage(_: AwaitingAnswer, _) =>
      for {
        _ <- ctx.broadcast(GameEvent.PlayerHitTheButton(m.playerId))
      } yield state
  }
}
