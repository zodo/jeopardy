package zodo.jeopardy.client.events

import korolev.Qsid
import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.actors.{GameActor, LobbyActor}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.{GameInfo, GameState}

object OutgoingProxy {

  case class State(playerName: Option[String])

  def handler(
    lobby: LobbyActorRef,
    session: Qsid,
    access: Access
  ): Actor.Stateful[AppEnv, State, ParametrizedClientEvent] =
    new Actor.Stateful[AppEnv, State, ParametrizedClientEvent] {

      override def receive[A](
        state: State,
        msg: ParametrizedClientEvent[A],
        context: actors.Context
      ): RIO[AppEnv, (State, A)] = {
        msg match {
          case ClientEvent.Introduce(name) =>
            for {
              _ <- log.debug(s"<- ClientEvent.Introduce($name)")
              _ <- access.transition(_.complete(ViewState.Authorized(name, None)))
            } yield State(Some(name))

          case ClientEvent.UploadFile(hash, pack) =>
            for {
              (id, _)                                 <- lobby ? LobbyActor.Message.NewGame(hash, pack)
              self: ActorRef[ParametrizedClientEvent] <- context.self
              _                                       <- self ! ClientEvent.EnterGame(id)
            } yield state
          case ClientEvent.EnterGame(gameId) =>
            state match {
              case State(Some(playerName)) =>
                for {
                  _         <- log.debug(s"entering game $gameId as ${playerName}")
                  maybeGame <- lobby ? LobbyActor.Message.GetGame(gameId)
                  _ <- maybeGame match {
                    case Some(game) =>
                      for {
                        gameListener <- context.make(
                          s"game-listener-$session",
                          actors.Supervisor.none,
                          (),
                          GameActorListener.handler(access)
                        )
                        _ <- access.transition(
                          _.complete(ViewState.InGame(GameInfo(gameId, "", Seq()), GameState.WaitingForStart))
                        )
                        _ <- game ! GameActor.InputMessage.JoinPlayer(session.toString, playerName, gameListener)
                      } yield ()
                    case None =>
                      access.transition(_.complete(ViewState.Authorized(playerName, Some("Game not found"))))
                  }
                } yield state
              case State(None) =>
                log.error(s"OutgoingProxy ~ entering game $gameId without name").as(state)
            }
          case ClientEvent.StartGame(gameId) =>
            for {
              maybeGame <- lobby ? LobbyActor.Message.GetGame(gameId)
              game      <- ZIO.fromOption(maybeGame).orElseFail(new Throwable("Game should exist"))
              _         <- game ! GameActor.InputMessage.StartGame
            } yield state
          case ClientEvent.ChooseQuestion(questionId) => ???
          case ClientEvent.PressAnswerButton          => ???
        }
      }.map(s => s -> ().asInstanceOf[A])
    }
}
