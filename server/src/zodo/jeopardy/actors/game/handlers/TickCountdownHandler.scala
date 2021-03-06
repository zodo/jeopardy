package zodo.jeopardy.actors.game.handlers

import zio.logging.log
import zodo.jeopardy.model.GameCommand.TickCountdown
import zodo.jeopardy.model.GameEvent
import zodo.jeopardy.model.GameEvent.{CountdownModel, CountdownUpdated}

object TickCountdownHandler extends Handler[TickCountdown] {
  override def process(m: TickCountdown, ctx: HandlerContext) = {
    case state =>
      state.countdowns.get(m.id) match {
        case Some(cd) =>
          if (cd.isFinished(m.tick)) {
            for {
              _ <- log.debug(s"counted to ${cd.max}! Id: ${m.id}")
              _ <- cd.postAction
              _ <- log.debug(s"executed post action! Id: ${m.id}")
              _ <- ctx.broadcast(GameEvent.CountdownUpdated(None))
            } yield state
              .withoutCd(m.id)
          } else {
            for {
              _ <- ctx.broadcast(CountdownUpdated(Some(CountdownModel(m.tick, cd.max))))
            } yield state.withCd(m.id, cd.copy(value = m.tick))
          }

        case None => log.error(s"can't find counter ${m.id} on tick ${m.tick}").as(state)
      }
  }
}
