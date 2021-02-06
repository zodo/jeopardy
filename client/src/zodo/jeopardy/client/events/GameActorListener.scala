package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.duration._
import zio.logging._
import zodo.jeopardy.actors.GameActor
import zodo.jeopardy.actors.GameActor.OutgoingMessage.SimpleStage.{InAwaitingAnswer, InRound}
import zodo.jeopardy.actors.GameActor.OutgoingMessage.{PlayerHitTheButton, PlayerListUpdated, SimpleStage, StageUpdated}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, Idle, ThinkingAboutAnswer}
import zodo.jeopardy.client.views.ViewState._

object GameActorListener {

  def handler(playerId: String, access: Access): Actor.Stateful[AppEnv, InGame, GameActor.OutgoingMessage] = {
    def transformView(newState: InGame) = access.maybeTransition { case _: InGame => newState }

    new Actor.Stateful[AppEnv, InGame, GameActor.OutgoingMessage] {

      override def receive[A](
        state: InGame,
        msg: GameActor.OutgoingMessage[A],
        context: actors.Context
      ): RIO[AppEnv, (InGame, A)] = {
        val stateF = msg match {
          case PlayerListUpdated(players) =>
            for {
              _ <- log.debug(s"GameActorListener <- NewPlayerConnected($players)")
              newPlayers = players.map(p =>
                PlayerInfo(
                  id = p.id,
                  name = p.name,
                  score = p.score,
                  state = calculatePlayerState(p.id, state.stage),
                  me = playerId == p.id
                )
              )
              newState = state.copy(players = newPlayers)
              _ <- transformView(newState)
            } yield newState
          case StageUpdated(stage) =>
            for {
              _ <- log.debug(s"GameActorListener <- StageUpdated($stage)")
              players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
              newState = state.copy(players = players, stage = stage)
              _ <- transformView(newState)
            } yield newState

          case PlayerHitTheButton(playerId, hit) =>
            val newState = state.copy(players = state.players.map(withButtonPressed(_, playerId, pressed = hit)))
            for {
              _    <- log.debug(s"GameActorListener <- PlayerHitTheButton($hit, $playerId")
              _    <- transformView(newState)
              self <- context.self[GameActor.OutgoingMessage]
              _    <- (self ! PlayerHitTheButton(playerId, hit = false)).delay(1.second).when(hit).fork
            } yield newState
        }

        for {
          newState <- stateF

        } yield newState -> ().asInstanceOf[A]
      }
    }
  }

  private def calculatePlayerState(playerId: String, stage: SimpleStage): PlayerState = stage match {
    case InRound(_, _, activePlayer) if activePlayer == playerId       => ChoosesQuestion
    case InAwaitingAnswer(_, activePlayer) if activePlayer == playerId => ThinkingAboutAnswer
    case _                                                             => Idle
  }

  private def withButtonPressed(player: PlayerInfo, playerId: String, pressed: Boolean) = {
    if (player.id == playerId && player.buttonPressed != pressed) {
      player.copy(buttonPressed = pressed)
    } else {
      player
    }
  }
}
