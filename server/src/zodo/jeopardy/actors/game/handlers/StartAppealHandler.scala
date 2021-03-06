package zodo.jeopardy.actors.game.handlers

import zio.ZIO
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Appeal, Idle}
import zodo.jeopardy.actors.game.State.Stage.{Round, RoundStage}
import zodo.jeopardy.model.GameCommand.StartAppeal
import zodo.jeopardy.model.{GameCommand, GameEvent}

object StartAppealHandler extends Handler[StartAppeal] {
  override def process(m: StartAppeal, ctx: HandlerContext) = {
    case state @ RoundStage(i: Idle, round: Round) =>
      (for {
        _      <- ZIO.fail(()).when(!round.previousAnswers.players.get(m.playerId).contains(false))
        _      <- ZIO.fail(()).when(round.previousAnswers.triedToAppeal.contains(m.playerId))
        model  <- ZIO.fromOption(round.previousAnswers.model)
        answer <- ZIO.fromOption(round.previousAnswers.answers.find(_.playerId == m.playerId).map(_.text))
        (cdId, cd) <- ctx.setCountdown(ctx.config.appealTimeout)(self =>
          ZIO.foreach(state.players)(p => self ! GameCommand.ResolveAppeal(p.id, resolution = false)).unit
        )
        newStage = round.copy(stage =
          Appeal(
            cdId,
            model,
            answer,
            m.playerId,
            Map()
          )
        )
        _ <- ctx.stoppedCountdown(i.cdId)
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state.withStage(newStage).withCd(cdId, cd).withoutCd(i.cdId)).orElseSucceed(state)
  }
}
