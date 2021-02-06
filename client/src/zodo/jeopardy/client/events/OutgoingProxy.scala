package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.GameActor.GameActorRef
import zodo.jeopardy.actors.GameActor.OutgoingMessage.SimpleStage.WaitingForStart
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.actors.LobbyActor.State.GameEntry
import zodo.jeopardy.actors.{GameActor, LobbyActor}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.InGame

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
              GameEntry(id, _, _)                     <- lobby ? LobbyActor.Message.NewGame(hash, pack)
              self: ActorRef[ParametrizedClientEvent] <- context.self
              _                                       <- self ! ClientEvent.EnterGame(id)
            } yield state
          case (State(Some(playerName), _), ClientEvent.EnterGame(gameId)) =>
            for {
              _         <- log.debug(s"entering game $gameId as ${playerName}")
              maybeGame <- lobby ? LobbyActor.Message.GetGameEntry(gameId)
              newState <- maybeGame match {
                case Some(GameEntry(gameId, packId, game)) =>
                  for {

                    newViewState <- UIO(InGame(gameId, packId, Nil, WaitingForStart))
                    gameListener <- context.make(
                      s"game-listener-$playerId",
                      actors.Supervisor.none,
                      newViewState,
                      GameActorListener.handler(playerId, access)
                    )
                    _ <- access.transition(_ => newViewState)
                    _ <- game ! GameActor.InputMessage.JoinPlayer(playerId, playerName, gameListener)
                  } yield state.copy(game = Some(game))
                case None =>
                  access
                    .transition(_ => ViewState.Authorized(playerName, Some("Game not found")))
                    .as(State(Some(playerName), None))
              }
            } yield newState
          case (State(_, Some(game)), ClientEvent.StartGame) =>
            (game ! GameActor.InputMessage.StartGame).as(state)
          case (State(_, Some(game)), ClientEvent.ChooseQuestion(questionId)) =>
            (game ! GameActor.InputMessage.ChooseQuestion(playerId, questionId)).as(state)
          case (State(_, Some(game)), ClientEvent.HitButton) =>
            (game ! GameActor.InputMessage.HitButton(playerId)).as(state)
          case (State(_, Some(game)), ClientEvent.Answer(value)) =>
            (game ! GameActor.InputMessage.Answer(playerId, value)).as(state)
          case s => log.error(s"unexpected transition OutgoingProxy <- $s").as(state)

        }
      }.map(s => s -> ().asInstanceOf[A])
    }
  }
}
