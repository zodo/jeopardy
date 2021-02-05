package zodo.jeopardy.client.views

import korolev.Context
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

class AnonymousView(val ctx: Context.Scope[AppTask, RootState, ViewState, ClientEvent]) {

  import ctx._
  import levsha.dsl._
  import html._

  private val nameInputId = elementId()

  def render(): DocumentNode = optimize {
    form(
      input(nameInputId, `type` := "text", placeholder := "Your name"),
      button("OK"),
      event("submit")(access =>
        for {
          name <- access.valueOf(nameInputId)
          _    <- access.publish(ClientEvent.Introduce(name))
        } yield ()
      )
    )
  }
}
