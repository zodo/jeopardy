package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.client.environment.AppEnv
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
          state <- access.state.collect(new Throwable("not in game")) { case s: InGame => s }

          _ <- msg match {
            case PlayerAdded(p) =>
              for {
                _ <- log.debug(s"GameActorListener <- NewPlayerConnected($p)")
                newPlayer = PlayerInfo(
                  id = p.id,
                  name = p.name,
                  score = 0,
                  state = calculatePlayerState(p.id, state.stage),
                  me = ownerPlayerId == p.id
                )
                newState = state.copy(players = state.players :+ newPlayer)
                _ <- access.syncTransition(_ => newState)
              } yield ()

            case PlayerScoreUpdated(p, diff) =>
              for {
                _ <- log.debug(s"GameActorListener <- PlayerScoreUpdated($diff, $p)")
                newState = state.withPlayers(_.id == p, p => p.copy(score = p.score + diff))
                _ <- access.syncTransition(_ => newState)
              } yield ()

            case StageUpdated(stage) =>
              for {
                _ <- log.debug(s"GameActorListener <- StageUpdated($stage)")
                players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
                newState = state.copy(players = players, stage = stage)
                _ <- access.syncTransition(_ => newState)
              } yield ()

            case PlayerHitTheButton(playerId) =>
              val newState = state.withPlayers(_.id == playerId, _.copy(buttonPressed = true))
              for {
                _ <- log.debug(s"GameActorListener <- PlayerHitTheButton($playerId")
                _ <- access.syncTransition(_ => newState)
              } yield ()

            case PlayerGaveAnswer(playerId, answer, isCorrect) =>
              val newState =
                state.withPlayers(_.id == playerId, _.copy(guess = Some(PlayerGuess(answer, isCorrect))))

              for {
                _ <- log.debug(s"GameActorListener <- PlayerHasAnswer($playerId, $answer, $isCorrect)")
                _ <- access.syncTransition(_ => newState)
              } yield ()
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
