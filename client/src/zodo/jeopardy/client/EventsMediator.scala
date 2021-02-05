package zodo.jeopardy.client

import korolev.effect.Effect
import korolev.{Context, Extension, Qsid}
import zio._
import zio.actors.Actor.Stateful
import zio.actors.ActorRef
import zio.logging._
import zodo.jeopardy.client.AppState._
import zodo.jeopardy.client.actors.GameActor.OutgoingMessage.{NewPlayerConnected, RoundStarted}
import zodo.jeopardy.client.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.client.actors.{GameActor, LobbyActor}

final class EventsMediator(lobby: LobbyActorRef, ctx: Context[AppTask, AppState, ClientEvent])(implicit
  eff: Effect[AppTask]
) extends Extension[AppTask, AppState, ClientEvent] {

  type Access = Context.BaseAccess[AppTask, AppState, ClientEvent]

  object Scopes {

    val gameInfo = ctx.scope[GameInfo](
      read = { case InGame(gameInfo, _) => gameInfo },
      write = { case (s: InGame, gameInfo) => s.copy(gameInfo = gameInfo) }
    )
  }

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
              _ <- access.maybeTransition { case s @ InGame(GameInfo(_, _, players), _) =>
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
              }
            } yield () -> ()
          case RoundStarted(round) =>
            for {
              _ <- log.debug(s"GameActorListener <- RoundStarted")
              _ <- access
                .maybeTransition { case s: InGame =>
                  s.copy(gameState = InRound(round, Set()))
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
                _ <- access.transition(_ => AppState.Authorized(name, None))
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
                          _ <- access.transition(_ => AppState.InGame(GameInfo(gameId, "", Seq()), WaitingForStart))
                          _ <- game ! GameActor.InputMessage.JoinPlayer(session.toString, playerName, gameListener)
                        } yield ()
                      case None => access.transition(_ => AppState.Authorized(playerName, Some("Game not found")))
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

  def setup(access: Access): AppTask[Extension.Handlers[AppTask, AppState, ClientEvent]] = {
    for {
      session <- access.sessionId
      actorSystem <- DefaultActorSystem.system
      sessionProxyActor <- actorSystem.make(
        s"proxy-$session",
        actors.Supervisor.none,
        OutgoingProxy.State(None),
        OutgoingProxy(session, access)
      )
    } yield Extension.Handlers[AppTask, AppState, ClientEvent](
      onDestroy = () => sessionProxyActor.stop.unit,
      onMessage = sessionProxyActor ! _
    )
  }
}
