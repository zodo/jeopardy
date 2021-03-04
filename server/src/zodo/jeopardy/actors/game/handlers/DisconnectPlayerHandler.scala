package zodo.jeopardy.actors.game.handlers

import zio.{UIO, ZIO}
import zodo.jeopardy.model.{GameCommand, GameEvent, LobbyCommand}

object DisconnectPlayerHandler extends Handler[GameCommand.DisconnectPlayer] {
  override def process(m: GameCommand.DisconnectPlayer, ctx: HandlerContext) = {
    case state =>
      val idx = state.players.indexWhere(_.id == m.id)
      if (idx < 0) {
        UIO(state)
      } else {
        val player = state.players(idx)
        val newState = state.copy(players = state.players.updated(idx, player.copy(disconnected = true)))
        if (newState.players.forall(_.disconnected)) {
          for {
            _ <- ZIO.foreachPar_(state.countdowns)(_._2.fiber.interrupt)
            _ <- ctx.lobby ! LobbyCommand.EndGame(state.id)
          } yield newState
        } else {
          for {
            _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          } yield newState
        }
      }
  }
}
