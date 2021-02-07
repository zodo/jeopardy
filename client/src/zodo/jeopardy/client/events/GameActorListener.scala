package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, Idle, ThinkingAboutAnswer}
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.GameEvent._
import zodo.jeopardy.model.{GameEvent, StageSnapshot}

object GameActorListener {

  def handler(ownerPlayerId: String, access: Access): Actor.Stateful[AppEnv, Unit, GameEvent] = {
    new Actor.Stateful[AppEnv, Unit, GameEvent] {

      override def receive[A](
        state: Unit,
        msg: GameEvent[A],
        context: actors.Context
      ): RIO[AppEnv, (Unit, A)] = {
        for {
          _     <- log.debug(s"GameActorListener <- $msg")
          state <- access.state.collect(new Throwable("not in game")) { case s: InGame => s }

          _ <- msg match {
            case PlayerAdded(p) =>
              val newPlayer = PlayerInfo(
                id = p.id,
                name = p.name,
                score = 0,
                state = calculatePlayerState(p.id, state.stage),
                me = ownerPlayerId == p.id
              )
              val newState = state.copy(players = state.players :+ newPlayer)
              access.syncTransition(_ => newState)

            case PlayerScoreUpdated(p, diff) =>
              val newState = state.withPlayers(_.id == p, p => p.copy(score = p.score + diff))
              access.syncTransition(_ => newState)

            case StageUpdated(stage) =>
              val players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
              val newState = state.copy(players = players, stage = stage)
              access.syncTransition(_ => newState)

            case PlayerHitTheButton(playerId) =>
              val newState = state.withPlayers(_.id == playerId, _.copy(buttonPressed = true))
              access.syncTransition(_ => newState)

            case PlayerGaveAnswer(playerId, answer, isCorrect) =>
              val newState = state
                .withPlayers(_.id == playerId, _.copy(guess = Some(PlayerGuess(answer, isCorrect))))
              access.syncTransition(_ => newState)

            case CountdownUpdated(v) =>
              val newState = state.copy(countdown = v.map(v => ViewState.Countdown(v.value, v.max)))
              access.syncTransition(_ => newState)

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
