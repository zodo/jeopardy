package zodo.jeopardy.client.views

import korolev.Context
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

class AnonymousView(val ctx: Context[AppTask, ViewState, ClientEvent]) {

  import ctx._
  import levsha.dsl._
  import html._

  private val nameInputId = elementId()

  def render(): DocumentNode = optimize {
    div(
      clazz := "center-container",
      div(
        clazz := "nes-container is-rounded",
        form(
          clazz := "nes-field login-form",
          label(
            `for` := "name_field",
            "Your name"
          ),
          input(
            nameInputId,
            `type` := "text",
            clazz := "nes-input"
          ),
          button(
            clazz := "nes-btn is-success",
            "OK"
          ),
          event("submit")(access =>
            for {
              name <- access.valueOf(nameInputId)
              _    <- access.publish(ClientEvent.Introduce(name))
            } yield ()
          )
        )
      )
    )
  }
}
