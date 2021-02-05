package zodo.jeopardy.client

import korolev.Context
import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import zio.actors.Supervisor
import zodo.jeopardy.actors.LobbyActor
import zodo.jeopardy.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.client.views.RootView

import scala.concurrent.ExecutionContext

class KorolevService(implicit eff: Effect[AppTask], ec: ExecutionContext) {

  val configM = for {
    system <- DefaultActorSystem.system
    lobby <- system.make("lobby", Supervisor.none, LobbyActor.State.init, LobbyActor.handler)
  } yield config(lobby)

  val ctx = Context[AppTask, RootState, ClientEvent]

  import levsha.dsl._
  import html._

  private def config(lobby: LobbyActorRef) = {

    val rootView = new RootView(
      ctx.scope[AppState](
        read = { case RootState(_, appState) => appState },
        write = { case (rootState, appState) => rootState.copy(appState = appState) }
      )
    )

    val eventsMediator = new EventsMediator(lobby)

    KorolevServiceConfig[AppTask, RootState, ClientEvent](
      stateLoader = StateLoader.default(RootState(isLoading = false, AppState.Anonymous)),
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
              if (state.isLoading) cursor @= "wait" else void,
              rootView.render(state.appState)
            )
          )
        }
    )
  }

}
