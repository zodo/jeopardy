package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import zodo.jeopardy.client.components.Uploader
import korolev.state.javaSerialization._
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

class AuthorizedView(val ctx: Context[AppTask, ViewState, ClientEvent])(implicit
  eff: Effect[AppTask]
) {

  import ctx._
  import levsha.dsl._
  import html._

  private val gameInputId = elementId()
  private val uploader = new Uploader

  def render(s: ViewState.Authorized): DocumentNode = optimize {
    div(
      clazz := "center-container",
      div(
        uploader(()) {
          case (access, Uploader.FileUploaded(hash, pack)) =>
            access.publish(ClientEvent.UploadFile(hash, pack))
        },
        div(
          clazz := "nes-container with-title is-centered margin-top",
          p(
            clazz := "title",
            "Enter game ID"
          ),
          form(
            input(gameInputId, `class` := "nes-input", `type` := "text"),
            button(`class` := "nes-btn is-success", "Join"),
            event("submit")(access =>
              for {
                gameId <- access.valueOf(gameInputId)
                _      <- access.publish(ClientEvent.EnterGame(gameId))
              } yield ()
            )
          )
        ),
        s.errorMessage match {
          case Some(msg) =>
            p(
              backgroundColor @= "red",
              msg
            )
          case None => void
        }
      )
    )
  }
}
