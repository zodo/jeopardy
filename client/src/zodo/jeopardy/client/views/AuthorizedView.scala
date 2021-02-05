package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import zodo.jeopardy.client.components.Uploader
import zodo.jeopardy.client.{AppState, AppTask, ClientEvent, RootState}
import korolev.state.javaSerialization._

class AuthorizedView(val ctx: Context.Scope[AppTask, RootState, AppState, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  val gameInputId = elementId()
  val uploader = new Uploader

  def render(s: AppState.Authorized) = optimize {
    div(
      h2(s"Hello ${s.name}!"),
      div(
        h3("Either create a new game"),
        uploader(()) { case (access, Uploader.FileUploaded(hash, pack)) =>
          access.publish(ClientEvent.UploadFile(hash, pack))
        },
        h3("Or join the existing"),
        form(
          input(gameInputId, `type` := "text", placeholder := "Game ID"),
          button("Join"),
          event("submit")(access =>
            for {
              gameId <- access.valueOf(gameInputId)
              _ <- access.publish(ClientEvent.EnterGame(gameId))
            } yield ()
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
