package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.{GameActorRef, LobbyActorRef}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.InGame
import zodo.jeopardy.model.StageSnapshot.BeforeStart
import zodo.jeopardy.model.{GameCommand, GameEntry, LobbyCommand, PlayerId}

object SessionActor {

  case class State(playerName: Option[String], gameToConnectId: Option[String], game: Option[GameActorRef])

  def handler(
    lobby: LobbyActorRef,
    playerId: PlayerId,
    access: Access
  ): Actor.Stateful[AppEnv, State, ParametrizedClientEvent] = {

    new Actor.Stateful[AppEnv, State, ParametrizedClientEvent] {

      override def receive[A](
        state: State,
        msg: ParametrizedClientEvent[A],
        context: actors.Context
      ): RIO[AppEnv, (State, A)] = {
        (state, msg) match {
          case (State(_, gameToConnectId, _), ClientEvent.Introduce(name)) =>
            for {
              _                                       <- log.debug(s"<- ClientEvent.Introduce($name)")
              _                                       <- access.transition(_ => ViewState.Authorized(name))
              self: ActorRef[ParametrizedClientEvent] <- context.self
              _ <- ZIO
                .fromOption(gameToConnectId)
                .flatMap(id => self ! ClientEvent.EnterGame(id))
                .ignore
            } yield state.copy(playerName = Some(name))

          case (State(_, _, maybeGame), ClientEvent.Leave) =>
            (for {
              _    <- lobby ? LobbyCommand.RemovePlayer(playerId)
              game <- ZIO.fromOption(maybeGame)
              _    <- game ? GameCommand.DisconnectPlayer(playerId)
            } yield ()).ignore.as(state)

          case (_, ClientEvent.UploadFile(hash, pack)) =>
            for {
              GameEntry(id, _, _)                     <- lobby ? LobbyCommand.NewGame(hash, pack)
              self: ActorRef[ParametrizedClientEvent] <- context.self
              _                                       <- self ! ClientEvent.EnterGame(id)
            } yield state

          case (State(None, _, _), ClientEvent.EnterGame(gameId)) =>
            for {
              _ <- log.debug(s"entering game $gameId as anonymous")
              _ <- access.syncTransition(_ => ViewState.Anonymous)
            } yield state.copy(gameToConnectId = Some(gameId))

          case (State(Some(playerName), _, _), ClientEvent.EnterGame(gameId)) =>
            for {
              _         <- log.debug(s"entering game $gameId as ${playerName}")
              maybeGame <- lobby ? LobbyCommand.GetGameEntry(gameId)
              newState <- maybeGame match {
                case Some(GameEntry(gameId, packId, game)) =>
                  for {
                    stateUpdater <- context.make(
                      s"state-updater",
                      actors.Supervisor.none,
                      (),
                      ViewStateUpdaterActor.handler(access)
                    )
                    gameListener <- context.make(
                      s"game-listener",
                      actors.Supervisor.none,
                      (),
                      GameActorListener.handler(playerName, playerId, stateUpdater)
                    )
                    _ <- access.transition(_ => InGame(gameId, packId, Nil, BeforeStart, None))
                    _ <- game ! GameCommand.AddPlayer(playerId, playerName, gameListener)
                  } yield state.copy(game = Some(game))
                case None =>
                  access
                    .transition(_ => ViewState.Authorized(playerName, Some("Game not found")))
                    .as(state)
              }
            } yield newState
          case (State(_, _, Some(game)), ClientEvent.StartGame) =>
            (game ! GameCommand.Start).as(state)
          case (State(_, _, Some(game)), ClientEvent.SelectQuestion(questionId)) =>
            (game ! GameCommand.SelectQuestion(playerId, questionId)).as(state)
          case (State(_, _, Some(game)), ClientEvent.HitButton) =>
            (game ! GameCommand.HitButton(playerId)).as(state)
          case (State(_, _, Some(game)), ClientEvent.GiveAnswer(value)) =>
            (game ! GameCommand.GiveAnswer(playerId, value)).as(state)
          case s => log.error(s"unexpected transition OutgoingProxy <- $s").as(state)

        }
      }.map(s => s -> ().asInstanceOf[A])
    }
  }
}
