package zodo.jeopardy.actors.game.handlers

import zodo.jeopardy.actors.game.State.Player
import zodo.jeopardy.model.{GameCommand, GameEvent}

object AddPlayerHandler extends Handler[GameCommand.AddPlayer] {
  override def process(m: GameCommand.AddPlayer, ctx: HandlerContext) = {
    case state =>
      val idx = state.players.indexWhere(_.id == m.id)
      if (idx >= 0) {
        val player = state.players(idx).copy(reply = m.reply, disconnected = false)
        val newState = state.copy(players = state.players.updated(idx, player))
        for {
          _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          _ <- player.reply ! GameEvent.StageUpdated(state.stage.toSnapshot)
        } yield newState
      } else {
        val player = Player(m.id, m.name, 0, m.reply)
        val newState = state.copy(players = state.players :+ player)
        for {
          _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          _ <- player.reply ! GameEvent.StageUpdated(state.stage.toSnapshot)
        } yield newState
      }
  }
}
