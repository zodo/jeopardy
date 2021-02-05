package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.GameActor.GameActorRef
import zodo.jeopardy.actors.GameActor.State.Stage.WaitingForStart
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
              _ <- access.transition(_.complete(ViewState.Authorized(name)))
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
                      GameViewStateUpdater.handler(playerId, access)
                    )
                    _ <- access.transition(
                      _.complete(newViewState)
                    )
                    _ <- game ! GameActor.InputMessage.JoinPlayer(playerId, playerName, gameListener)
                  } yield state.copy(game = Some(game))
                case None =>
                  access
                    .transition(_.complete(ViewState.Authorized(playerName, Some("Game not found"))))
                    .as(State(Some(playerName), None))
              }
            } yield newState
          case (State(None, _), ClientEvent.EnterGame(gameId)) =>
            log.error(s"OutgoingProxy ~ entering game $gameId without name").as(state)
          case (_, ClientEvent.StartGame(gameId)) =>
            for {
              maybeEntry            <- lobby ? LobbyActor.Message.GetGameEntry(gameId)
              GameEntry(_, _, game) <- ZIO.fromOption(maybeEntry).orElseFail(new Throwable("Game should exist"))
              _                     <- game ! GameActor.InputMessage.StartGame
            } yield state
          case (State(_, Some(game)), ClientEvent.ChooseQuestion(questionId)) =>
            (game ! GameActor.InputMessage.ChooseQuestion(playerId, questionId)).as(state)
          case (State(_, _), _: ClientEvent.ChooseQuestion) =>
            log.error(s"OutgoingProxy ~ choosing question without game").as(state)
          case (_, ClientEvent.PressAnswerButton) => ???
        }
      }.map(s => s -> ().asInstanceOf[A])
    }
  }
}
