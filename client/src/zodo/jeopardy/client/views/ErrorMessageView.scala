package zodo.jeopardy.client.views

import korolev.Context
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

class ErrorMessageView(val ctx: Context[AppTask, ViewState, ClientEvent]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(s: ViewState.ErrorMessage): DocumentNode = optimize {
    div(
      backgroundColor @= "red",
      p(s.msg)
    )
  }
}
