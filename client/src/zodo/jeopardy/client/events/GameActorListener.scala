package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.duration._
import zio.logging._
import zodo.jeopardy.actors.GameActor
import zodo.jeopardy.actors.GameActor.OutgoingMessage.SimpleStage.{InAwaitingAnswer, InRound}
import zodo.jeopardy.actors.GameActor.OutgoingMessage.{
  NewPlayerConnected,
  PlayerHasAnswer,
  PlayerHitTheButton,
  PlayerScoreUpdated,
  SimpleStage,
  StageUpdated
}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, Idle, ThinkingAboutAnswer}
import zodo.jeopardy.client.views.ViewState._

object GameActorListener {

  def handler(ownerPlayerId: String, access: Access): Actor.Stateful[AppEnv, InGame, GameActor.OutgoingMessage] = {
    new Actor.Stateful[AppEnv, InGame, GameActor.OutgoingMessage] {

      override def receive[A](
        state: InGame,
        msg: GameActor.OutgoingMessage[A],
        context: actors.Context
      ): RIO[AppEnv, (InGame, A)] = {
        val stateF = msg match {
          case NewPlayerConnected(p) =>
            for {
              _ <- log.debug(s"GameActorListener <- NewPlayerConnected($p)")
              newPlayer = PlayerInfo(
                id = p.id,
                name = p.name,
                score = p.score,
                state = calculatePlayerState(p.id, state.stage),
                me = ownerPlayerId == p.id
              )
              newState = state.copy(players = state.players :+ newPlayer)
              _ <- access.transition(_ => newState)
            } yield newState
          case PlayerScoreUpdated(p, diff) =>
            for {
              _ <- log.debug(s"GameActorListener <- PlayerScoreUpdated($diff, $p)")
              newState = state.withPlayers(_.id == p, p => p.copy(score = p.score + diff))
              _ <- access.transition(_ => newState)
            } yield newState
          case StageUpdated(stage) =>
            for {
              _ <- log.debug(s"GameActorListener <- StageUpdated($stage)")
              players = state.players.map(p => p.copy(state = calculatePlayerState(p.id, stage)))
              newState = state.copy(players = players, stage = stage)
              _ <- access.transition(_ => newState)
            } yield newState

          case PlayerHitTheButton(playerId) =>
            val newState = state.withPlayers(_.id == playerId, _.copy(buttonPressed = true))
            for {
              _ <- log.debug(s"GameActorListener <- PlayerHitTheButton($playerId")
              _ <- access.transition(_ => newState)
            } yield newState

          case PlayerHasAnswer(playerId, answer, isCorrect) =>
            val newState =
              state.withPlayers(_.id == playerId, _.copy(guess = Some(PlayerGuess(answer, isCorrect))))

            for {
              _ <- log.debug(s"GameActorListener <- PlayerHasAnswer($playerId, $answer, $isCorrect)")
              _ <- access.transition(_ => newState)
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
}
