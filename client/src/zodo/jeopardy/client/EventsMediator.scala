package zodo.jeopardy.client

import korolev.effect.Effect
import korolev.{Context, Extension, Qsid}
import zio._
import zio.actors.Actor.Stateful
import zio.actors.ActorRef
import zio.logging._
import zodo.jeopardy.actors.{GameActor, LobbyActor}
import zodo.jeopardy.client.AppState._
import zodo.jeopardy.actors.GameActor.OutgoingMessage.{NewPlayerConnected, RoundStarted}
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef

final class EventsMediator(lobby: LobbyActorRef)(implicit
  eff: Effect[AppTask]
) extends Extension[AppTask, RootState, ClientEvent] {

  type Access = Context.BaseAccess[AppTask, RootState, ClientEvent]

  object GameActorListener {

    def apply(access: Access): Stateful[AppEnv, Unit, GameActor.OutgoingMessage] =
      new Stateful[AppEnv, Unit, GameActor.OutgoingMessage] {

        override def receive[A](
          state: Unit,
          msg: GameActor.OutgoingMessage[A],
          context: actors.Context
        ): RIO[AppEnv, (Unit, A)] = msg match {
          case NewPlayerConnected(id, name) =>
            for {
              _ <- log.debug(s"GameActorListener <- NewPlayerConnected($id, $name)")
              session <- access.sessionId
              _ <- access.maybeTransition { case r @ RootState(_, s @ InGame(GameInfo(_, _, players), _)) =>
                r.complete(
                  s.copy(gameInfo =
                    s.gameInfo.copy(players =
                      players :+ PlayerInfo(
                        id,
                        name,
                        0,
                        PlayerState.Idle,
                        session.toString == id
                      )
                    )
                  )
                )
              }
            } yield () -> ()
          case RoundStarted(round) =>
            for {
              _ <- log.debug(s"GameActorListener <- RoundStarted")
              _ <- access
                .maybeTransition { case r @ RootState(_, s: InGame) =>
                  r.complete(s.copy(gameState = InRound(round, Set())))
                }
            } yield () -> ()
        }
      }
  }

  object OutgoingProxy {
    case class State(playerName: Option[String])

    def apply(session: Qsid, access: Access): Stateful[AppEnv, State, ParametrizedClientEvent] =
      new Stateful[AppEnv, State, ParametrizedClientEvent] {

        override def receive[A](
          state: State,
          msg: ParametrizedClientEvent[A],
          context: actors.Context
        ): RIO[AppEnv, (State, A)] = {
          msg match {
            case ClientEvent.Introduce(name) =>
              for {
                _ <- log.debug(s"<- ClientEvent.Introduce($name)")
                _ <- access.transition(_.complete(AppState.Authorized(name, None)))
              } yield State(Some(name))

            case ClientEvent.UploadFile(hash, pack) =>
              for {
                (id, _) <- lobby ? LobbyActor.Message.NewGame(hash, pack)
                self: ActorRef[ParametrizedClientEvent] <- context.self
                _ <- self ! ClientEvent.EnterGame(id)
              } yield state
            case ClientEvent.EnterGame(gameId) =>
              state match {
                case State(Some(playerName)) =>
                  for {
                    _ <- log.debug(s"entering game $gameId as ${playerName}")
                    maybeGame <- lobby ? LobbyActor.Message.GetGame(gameId)
                    _ <- maybeGame match {
                      case Some(game) =>
                        for {
                          gameListener <- context.make(
                            s"game-listener-$session",
                            actors.Supervisor.none,
                            (),
                            GameActorListener(access)
                          )
                          _ <- access.transition(
                            _.complete(AppState.InGame(GameInfo(gameId, "", Seq()), WaitingForStart))
                          )
                          _ <- game ! GameActor.InputMessage.JoinPlayer(session.toString, playerName, gameListener)
                        } yield ()
                      case None =>
                        access.transition(_.complete(AppState.Authorized(playerName, Some("Game not found"))))
                    }
                  } yield state
                case State(None) =>
                  log.error(s"OutgoingProxy ~ entering game $gameId without name").as(state)
              }
            case ClientEvent.StartGame(gameId) =>
              for {
                maybeGame <- lobby ? LobbyActor.Message.GetGame(gameId)
                game <- ZIO.fromOption(maybeGame).orElseFail(new Throwable("Game should exist"))
                _ <- game ! GameActor.InputMessage.StartGame
              } yield state
            case ClientEvent.ChooseQuestion(questionId) => ???
            case ClientEvent.PressAnswerButton          => ???
          }
        }.map(s => s -> ().asInstanceOf[A])
      }
  }

  def setup(
    access: Context.BaseAccess[AppTask, RootState, ClientEvent]
  ): AppTask[Extension.Handlers[AppTask, RootState, ClientEvent]] = {
    for {
      session <- access.sessionId
      actorSystem <- DefaultActorSystem.system
      sessionProxyActor <- actorSystem.make(
        s"proxy-$session",
        actors.Supervisor.none,
        OutgoingProxy.State(None),
        OutgoingProxy(session, access)
      )
    } yield {
      Extension.Handlers[AppTask, RootState, ClientEvent](
        onDestroy = () => sessionProxyActor.stop.unit,
        onMessage = message =>
          for {
            _ <- access.transition(_.loading)
            _ <- sessionProxyActor ! message
          } yield ()
      )
    }
  }
}
