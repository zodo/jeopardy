package zodo.jeopardy.client.events

import korolev.Extension
import korolev.effect.Effect
import zio._
import zio.actors.ActorSystem
import zio.logging.log
import zodo.jeopardy.actors.LobbyActorRef
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.model.LobbyCommand

final class SessionSetup(system: ActorSystem, lobby: LobbyActorRef)(implicit
  eff: Effect[AppTask]
) extends Extension[AppTask, ViewState, ClientEvent] {

  def setup(access: Access): AppTask[Extension.Handlers[AppTask, ViewState, ClientEvent]] = {
    for {
      playerId <- access.sessionId.map(_.deviceId)
      res      <- lobby ? LobbyCommand.AddPlayer(playerId)
      handlers <- res match {
        case Right(()) =>
          system
            .make(
              s"session-$playerId",
              actors.Supervisor.none,
              SessionActor.State(None, None, None),
              SessionActor.handler(lobby, playerId, access)
            )
            .map(session =>
              Extension.Handlers[AppTask, ViewState, ClientEvent](
                onDestroy = () =>
                  for {
                    _ <- log.debug(s"Started session destroying $playerId")
                    _ <- session ? ClientEvent.Leave
                    _ <- session.stop
                    _ <- log.debug(s"Finished session destroying $playerId")
                  } yield (),
                onMessage = message => session ! message
              )
            )
        case Left(msg) =>
          access.transition(_ => ViewState.ErrorMessage(msg)).as(Extension.Handlers[AppTask, ViewState, ClientEvent]())
      }
    } yield handlers
  }
}
