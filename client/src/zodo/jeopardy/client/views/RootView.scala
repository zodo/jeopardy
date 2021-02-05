package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.client.views.ViewState._

class RootView(val ctx: Context.Scope[AppTask, RootState, ViewState, ClientEvent])(implicit eff: Effect[AppTask]) {

  import levsha.dsl._
  import html._

  private val anonymousView = new AnonymousView(ctx)
  private val authorizedView = new AuthorizedView(ctx)

  private val inGameView = new InGameView(
    ctx.scope(
      read = { case RootState(_, s: InGame) => s },
      write = { case (s, inGame) => s.copy(viewState = inGame) }
    )
  )

  def render(state: ViewState) = optimize {
    div(
      TagDef("main")(
        style := "width: 1000px; margin: 20px auto",
        state match {
          case ViewState.Anonymous     => anonymousView.render()
          case s: ViewState.Authorized => authorizedView.render(s)
          case s: ViewState.InGame     => inGameView.render(s)
        }
      )
    )
  }
}
