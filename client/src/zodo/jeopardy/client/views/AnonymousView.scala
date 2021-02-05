package zodo.jeopardy.client.views

import korolev.Context
import zodo.jeopardy.client.{AppState, AppTask, ClientEvent, RootState}

class AnonymousView(val ctx: Context.Scope[AppTask, RootState, AppState, ClientEvent]) {

  import ctx._
  import levsha.dsl._
  import html._

  private val nameInputId = elementId()

  def render() = optimize {
    form(
      input(nameInputId, `type` := "text", placeholder := "Your name"),
      button("OK"),
      event("submit")(access =>
        for {
          name <- access.valueOf(nameInputId)
          _ <- access.publish(ClientEvent.Introduce(name))
        } yield ()
      )
    )
  }
}
