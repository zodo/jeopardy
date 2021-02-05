package zodo.jeopardy.client.events

import korolev.effect.Effect
import korolev.{Context, Extension}
import zio._
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.client.environment.{AppTask, DefaultActorSystem}
import zodo.jeopardy.client.views.RootState

final class EventsMediator(lobby: LobbyActorRef)(implicit
  eff: Effect[AppTask]
) extends Extension[AppTask, RootState, ClientEvent] {

  def setup(
    access: Context.BaseAccess[AppTask, RootState, ClientEvent]
  ): AppTask[Extension.Handlers[AppTask, RootState, ClientEvent]] = {
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
