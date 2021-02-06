package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import zio.actors.Supervisor
import zodo.jeopardy.actors.{LobbyActor, LobbyActorRef}
import zodo.jeopardy.client.environment.{AppTask, DefaultActorSystem}
import zodo.jeopardy.client.events.{ClientEvent, EventsMediator}

import scala.concurrent.ExecutionContext

class KorolevService(implicit eff: Effect[AppTask], ec: ExecutionContext) {

  val configM = for {
    system <- DefaultActorSystem.system
    lobby  <- system.make("lobby", Supervisor.none, LobbyActor.State.init, LobbyActor.handler)
  } yield config(lobby)

  val ctx = Context[AppTask, ViewState, ClientEvent]

  import levsha.dsl._
  import html._

  private def config(lobby: LobbyActorRef) = {

    val rootView = new RootView(ctx)

    val eventsMediator = new EventsMediator(lobby)

    KorolevServiceConfig[AppTask, ViewState, ClientEvent](
      stateLoader = StateLoader.default(ViewState.Anonymous),
      extensions = List(eventsMediator),
      document = state =>
        optimize {
          Html(
            head(
              link(
                href := "https://cdn.jsdelivr.net/npm/picnic@6/picnic.min.css",
                rel := "stylesheet",
                `type` := "text/css"
              ),
              link(
                href := "/static/main.css",
                rel := "stylesheet",
                `type` := "text/css"
              )
            ),
            body(
              rootView.render(state)
            )
          )
        }
    )
  }
}
