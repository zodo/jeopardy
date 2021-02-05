package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.GameActor
import zodo.jeopardy.actors.GameActor.OutgoingMessage.{PlayerListUpdated, StageUpdated}
import zodo.jeopardy.actors.GameActor.State.Stage
import zodo.jeopardy.actors.GameActor.State.Stage.InRound
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.RootState
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, Idle}
import zodo.jeopardy.client.views.ViewState._

object GameViewStateUpdater {

  def handler(playerId: String, access: Access): Actor.Stateful[AppEnv, InGame, GameActor.OutgoingMessage] =
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
                  state = calculatePlayerState(p.id, state.gameStage),
                  me = playerId == p.id
                )
              )
            } yield state.copy(players = newPlayers)
          case StageUpdated(stage) =>
            for {
              _ <- log.debug(s"GameActorListener <- StageUpdated($stage)")
            } yield {
              val players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
              state.copy(
                players = players,
                gameStage = stage
              )
            }
        }

        for {
          newState <- stateF
          _ <- access.maybeTransition { case r @ RootState(_, _: InGame) =>
            r.complete(newState)
          }
        } yield newState -> ().asInstanceOf[A]
      }
    }

  private def calculatePlayerState(playerId: String, stage: Stage): PlayerState = stage match {
    case InRound(_, _, activePlayer) if playerId == activePlayer => ChoosesQuestion
    case _                                                       => Idle
  }
}
