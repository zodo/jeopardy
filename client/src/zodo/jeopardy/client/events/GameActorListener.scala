package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.duration.durationInt
import zio.logging._
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.events.ViewStateUpdaterActor.{UpdateGame, ViewStateUpdaterActorRef}
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, Idle, ThinkingAboutAnswer}
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.GameEvent._
import zodo.jeopardy.model.{GameEvent, PlayerId, StageSnapshot}

object GameActorListener {

  def handler(
    playerName: String,
    ownerPlayerId: PlayerId,
    updater: ViewStateUpdaterActorRef
  ): Actor.Stateful[AppEnv, Unit, GameEvent] = {
    new Actor.Stateful[AppEnv, Unit, GameEvent] {

      override def receive[A](
        state: Unit,
        msg: GameEvent[A],
        context: actors.Context
      ): RIO[AppEnv, (Unit, A)] = {
        for {
          _ <- log.debug(s"'$playerName' - GameActorListener <- $msg")

          _ <- msg match {
            case PlayersUpdated(players) =>
              updater ! UpdateGame(state => {
                val newPlayers = players.map(p =>
                  PlayerInfo(
                    id = p.id,
                    name = p.name,
                    score = p.score,
                    state = calculatePlayerState(p.id, state.stage),
                    me = ownerPlayerId == p.id,
                    disconnected = p.disconnected
                  )
                )
                state.copy(players = newPlayers)
              })

            case StageUpdated(stage) =>
              updater ! UpdateGame(state => {
                val players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
                state.copy(players = players, stage = stage)
              })

            case PlayerHitTheButton(playerId) =>
              def withButtonPressed(p: Boolean) =
                updater ! UpdateGame(_.withPlayerEvent(playerId, _.copy(buttonPressed = p)))
              for {
                _ <- withButtonPressed(true)
                _ <- withButtonPressed(false).delay(1.second).fork
              } yield ()

            case PlayerGaveAnswer(playerId, answer, isCorrect) =>
              def withGuess(g: Option[PlayerGuess]) =
                updater ! UpdateGame(_.withPlayerEvent(playerId, _.copy(guess = g)))
              for {
                _ <- withGuess(Some(PlayerGuess(answer, isCorrect)))
                _ <- withGuess(None).delay(2.seconds).fork
              } yield ()

            case CountdownUpdated(v) =>
              updater ! UpdateGame(_.copy(countdown = v.map(v => ViewState.Countdown(v.value, v.max))))
          }

        } yield () -> ().asInstanceOf[A]
      }
    }
  }

  private def calculatePlayerState(playerId: String, stage: StageSnapshot): PlayerState = stage match {
    case StageSnapshot.Round(_, _, activePlayer) if activePlayer == playerId      => ChoosesQuestion
    case StageSnapshot.AnswerAttempt(_, activePlayer) if activePlayer == playerId => ThinkingAboutAnswer
    case _                                                                        => Idle
  }
}
