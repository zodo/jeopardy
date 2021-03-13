package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.client.views.ViewState._

import scala.concurrent.duration.DurationInt

class RootView(val ctx: Context[AppTask, ViewState, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  private val anonymousView = new AnonymousView(ctx)
  private val errorMessageView = new ErrorMessageView(ctx)
  private val authorizedView = new AuthorizedView(ctx)

  private val inGameView = new InGameView(
    ctx.scope(
      read = { case s: InGame => s },
      write = { case (s, inGame) => inGame }
    )
  )

  def render(state: ViewState): DocumentNode = optimize {
    state match {
      case RedirectToGame(id) =>
        p(
          "Redirecting...",
          delay(1.seconds)(_.publish(ClientEvent.EnterGame(id)))
        )
      case s: ErrorMessage => errorMessageView.render(s)
      case Anonymous       => anonymousView.render()
      case s: Authorized   => authorizedView.render(s)
      case s: InGame       => inGameView.render(s)
    }
  }
}
