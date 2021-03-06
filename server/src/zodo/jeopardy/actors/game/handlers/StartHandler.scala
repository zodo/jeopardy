package zodo.jeopardy.actors.game.handlers

import zio._
import zodo.jeopardy.actors.game.State.Stage.{AnswersSummary, Round}
import zodo.jeopardy.actors.game.State.Stage.RoundStage.Idle
import zodo.jeopardy.model.{GameCommand, GameEvent}

object StartHandler extends Handler[Unit] {
  override def process(m: Unit, ctx: HandlerContext) = {
    case state =>
      val alivePlayers = state.players.filterNot(_.disconnected)
      for {
        randomActivePlayer <- random.nextIntBounded(alivePlayers.size).map(idx => alivePlayers(idx))
        _                  <- ctx.self ! GameCommand.ReturnToRound
        newStage = Round(
          Idle("dummy"),
          state.pack.rounds.head,
          Set(),
          randomActivePlayer.id,
          AnswersSummary(None, Seq())
        )
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state.withStage(newStage)
  }
}
