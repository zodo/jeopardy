package zodo.jeopardy.client.views

import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import korolev.{/, Context, Root, Router}
import zio.UIO
import zio.actors.{ActorSystem, Supervisor}
import zodo.jeopardy.actors.{LobbyActor, LobbyActorRef}
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.{ClientEvent, SessionSetup}

import scala.concurrent.ExecutionContext

class KorolevService(implicit eff: Effect[AppTask], ec: ExecutionContext) {

  val configM = for {
    system <- ActorSystem("default")
    lobby  <- system.make("lobby", Supervisor.none, LobbyActor.State.init, LobbyActor.handler)
  } yield config(system, lobby)

  val ctx = Context[AppTask, ViewState, ClientEvent]

  import levsha.dsl._
  import html._

  private def config(system: ActorSystem, lobby: LobbyActorRef) = {

    val rootView = new RootView(ctx)

    val eventsMediator = new SessionSetup(system, lobby)

    val router = Router[AppTask, ViewState](
      fromState = { case s: ViewState.InGame => Root / "game" / s.id },
      toState = { case Root / "game" / id => _ => UIO(ViewState.RedirectToGame(id)) }
    )

    KorolevServiceConfig[AppTask, ViewState, ClientEvent](
      stateLoader = StateLoader.default(ViewState.Anonymous),
      extensions = List(eventsMediator),
      router = router,
      document = state =>
        optimize {
          Html(
            head(
              link(
                href := "https://unpkg.com/nes.css@2.3.0/css/nes.min.css",
                rel := "stylesheet"
              ),
              link(
                href := "https://fonts.gstatic.com",
                rel := "preconnect"
              ),
              link(
                href := "https://fonts.googleapis.com/css2?family=Press+Start+2P&display=swap",
                rel := "stylesheet"
              ),
              link(
                href := "/static/main.css",
                rel := "stylesheet"
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
