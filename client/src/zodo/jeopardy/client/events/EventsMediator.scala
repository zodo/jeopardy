package zodo.jeopardy.client.events

import korolev.Extension
import korolev.effect.Effect
import zio._
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.client.environment.{AppTask, DefaultActorSystem}
import zodo.jeopardy.client.views.ViewState

final class EventsMediator(lobby: LobbyActorRef)(implicit
  eff: Effect[AppTask]
) extends Extension[AppTask, ViewState, ClientEvent] {

  def setup(access: Access): AppTask[Extension.Handlers[AppTask, ViewState, ClientEvent]] = {
    for {
      session     <- access.sessionId
      actorSystem <- DefaultActorSystem.system
      sessionProxyActor <- actorSystem.make(
        s"proxy-$session",
        actors.Supervisor.none,
        OutgoingProxy.State(None, None),
        OutgoingProxy.handler(lobby, session.toString, access)
      )
    } yield {
      Extension.Handlers[AppTask, ViewState, ClientEvent](
        onDestroy = () => sessionProxyActor.stop.unit,
        onMessage = message => sessionProxyActor ! message
      )
    }
  }
}
