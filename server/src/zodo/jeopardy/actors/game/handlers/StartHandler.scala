package zodo.jeopardy.actors.game.handlers

import zio._
import zodo.jeopardy.actors.game.State.Stage.Round
import zodo.jeopardy.actors.game.State.Stage.RoundStage.Idle
import zodo.jeopardy.model.{GameCommand, GameEvent}

object StartHandler extends Handler[Unit] {
  override def process(m: Unit, ctx: HandlerContext) = {
    case state =>
      val alivePlayers = state.players.filterNot(_.disconnected)
      for {
        randomActivePlayer <- random.nextIntBounded(alivePlayers.size).map(idx => alivePlayers(idx))
        (cdId, cd)         <- ctx.setCountdown(ctx.config.questionSelectionTimeout)(_ ! GameCommand.ChooseRandomQuestion)
        newStage = Round(Idle(cdId), state.pack.rounds.head, Set(), randomActivePlayer.id)
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state
        .withCd(cdId, cd)
        .withStage(newStage)
  }
}
