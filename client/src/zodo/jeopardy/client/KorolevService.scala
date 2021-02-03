package zodo.jeopardy.client

import korolev.Context
import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import zodo.jeopardy.client.AppEvent.FileProcessed
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.Image

import scala.concurrent.ExecutionContext

class KorolevService(implicit eff: Effect[EnvTask], ec: ExecutionContext) {

  val ctx = Context[EnvTask, AppState, Any]

  import ctx._
  import levsha.dsl._
  import html._

  private val uploader = new Uploader

  val config = KorolevServiceConfig[EnvTask, AppState, Any](
    stateLoader = StateLoader.default(AppState.BeforeStart),
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
            TagDef("main")(
              style := "width: 1000px; margin: 0 auto",
              state match {
                case AppState.BeforeStart =>
                  uploader(()) { (access, event) =>
                    event match {
                      case FileProcessed(hash, pack) =>
                        access.transition(_ => AppState.ShowInfo(hash, pack))
                    }
                  }
                case AppState.ShowInfo(hash, pack) =>
                  div(
                    header(
                      button(
                        "Upload new",
                        event("click")(_.transition(_ => AppState.BeforeStart))
                      )
                    ),
                    pack.rounds.map(round =>
                      div(
                        h2(round.name),
                        when(round.typ == PackModel.RoundType.Final)(span("Final!")),
                        tbody(
                          round.themes.map(theme =>
                            tr(
                              style := "cursor: pointer",
                              td(theme.name),
                              theme.questions.map(question =>
                                td(
                                  question.price.toString(),
                                  event("click")(_.transition(s => AppState.QuestionDetails(hash, s, question)))
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )

                case AppState.QuestionDetails(hash, prev, question) =>
                  div(
                    header(
                      button(
                        "Back",
                        event("click")(access =>
                          access.maybeTransition { case AppState.QuestionDetails(_, prev, _) => prev }
                        )
                      )
                    ),
                    h2("Question"),
                    ul(
                      question.fragments.map(fragment =>
                        li(
                          fragment match {
                            case Image(url) =>
                              img(
                                width @= "600px",
                                src := s"http://localhost:8080/media/$hash/Images/${url.drop(1)}"
                              )
                            case _ => fragment.toString
                          }
                        )
                      )
                    ),
                    h2("Answer"),
                    ul(question.answers.correct.map(answer => li(answer)))
                  )
                case AppState.Error(message) =>
                  div(
                    header(
                      button(
                        "Try again",
                        event("click")(_.transition(_ => AppState.BeforeStart))
                      )
                    ),
                    p(
                      span(
                        `class` := "label error",
                        "Not OK"
                      ),
                      message
                    )
                  )
              }
            )
          )
        )
      }
  )

}
