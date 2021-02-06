package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.{GameActorRef, LobbyActor, LobbyActorRef}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.InGame
import zodo.jeopardy.model.{GameCommand, GameEntry, LobbyCommand}
import zodo.jeopardy.model.StageSnapshot.BeforeStart

object OutgoingProxy {

  case class State(playerName: Option[String], game: Option[GameActorRef])

  def handler(
    lobby: LobbyActorRef,
    playerId: String,
    access: Access
  ): Actor.Stateful[AppEnv, State, ParametrizedClientEvent] = {

    new Actor.Stateful[AppEnv, State, ParametrizedClientEvent] {

      override def receive[A](
        state: State,
        msg: ParametrizedClientEvent[A],
        context: actors.Context
      ): RIO[AppEnv, (State, A)] = {
        (state, msg) match {
          case (_, ClientEvent.Introduce(name)) =>
            for {
              _ <- log.debug(s"<- ClientEvent.Introduce($name)")
              _ <- access.transition(_ => ViewState.Authorized(name))
            } yield State(Some(name), None)

          case (_, ClientEvent.UploadFile(hash, pack)) =>
            for {
              GameEntry(id, _, _)                     <- lobby ? LobbyCommand.NewGame(hash, pack)
              self: ActorRef[ParametrizedClientEvent] <- context.self
              _                                       <- self ! ClientEvent.EnterGame(id)
            } yield state
          case (State(Some(playerName), _), ClientEvent.EnterGame(gameId)) =>
            for {
              _         <- log.debug(s"entering game $gameId as ${playerName}")
              maybeGame <- lobby ? LobbyCommand.GetGameEntry(gameId)
              newState <- maybeGame match {
                case Some(GameEntry(gameId, packId, game)) =>
                  for {
                    gameListener <- context.make(
                      s"game-listener-$playerId",
                      actors.Supervisor.none,
                      (),
                      GameActorListener.handler(playerId, access)
                    )
                    _ <- access.transition(_ => InGame(gameId, packId, Nil, BeforeStart))
                    _ <- game ! GameCommand.AddPlayer(playerId, playerName, gameListener)
                  } yield state.copy(game = Some(game))
                case None =>
                  access
                    .transition(_ => ViewState.Authorized(playerName, Some("Game not found")))
                    .as(State(Some(playerName), None))
              }
            } yield newState
          case (State(_, Some(game)), ClientEvent.StartGame) =>
            (game ! GameCommand.Start).as(state)
          case (State(_, Some(game)), ClientEvent.SelectQuestion(questionId)) =>
            (game ! GameCommand.SelectQuestion(playerId, questionId)).as(state)
          case (State(_, Some(game)), ClientEvent.HitButton) =>
            (game ! GameCommand.HitButton(playerId)).as(state)
          case (State(_, Some(game)), ClientEvent.GiveAnswer(value)) =>
            (game ! GameCommand.GiveAnswer(playerId, value)).as(state)
          case s => log.error(s"unexpected transition OutgoingProxy <- $s").as(state)

        }
      }.map(s => s -> ().asInstanceOf[A])
    }
  }
}
