package zodo.jeopardy.client

import korolev.Router
import korolev.web.PathAndQuery._
import korolev.Context
import korolev.akka._
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import korolev.zio.taskEffectInstance
import zio.{Runtime, Task, ZIO}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.internal.util.FileUtils
import zio.IO
import scala.xml.XML
import zodo.jeopardy.siq.SiqXmlContentParser
import zodo.jeopardy.model.PackModel
import zio.UIO
import zodo.jeopardy.model.PackModel.Fragment.Audio
import zodo.jeopardy.client.Uploader

import scala.concurrent.duration.DurationInt
import korolev.Component
import zodo.jeopardy.client.AppEvent.FileProcessed

object Entrypoint extends SimpleAkkaHttpKorolevApp {
  implicit val runtime = Runtime.default
  implicit val effect = taskEffectInstance(runtime)

  val ctx = Context[Task, AppState, Any]

  import ctx._

  import levsha.dsl._
  import html._

  def service: AkkaHttpService = akkaHttpService {
    KorolevServiceConfig[Task, AppState, Any](
      stateLoader = StateLoader.default(AppState.BeforeStart),
      document = state =>
        optimize {
          Html(
            head(
              link(
                href := "https://cdn.jsdelivr.net/npm/picnic@6/picnic.min.css",
                rel := "stylesheet",
                `type` := "text/css"
              )
            ),
            body(
              TagDef("main")(
                style := "width: 1000px; margin: 0 auto",
                state match {
                  case AppState.BeforeStart =>
                    Uploader.component(effect)(()) { (access, event) =>
                      event match {
                        case FileProcessed(pack) => access.transition(_ => AppState.ShowInfo(pack))
                      }
                    }
                  case AppState.ShowInfo(pack) =>
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
                                    event("click")(_.transition(s => AppState.QuestionDetails(s, question)))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )

                  case AppState.QuestionDetails(prev, question) =>
                    div(
                      header(
                        button(
                          "Back",
                          event("click")(access =>
                            access.maybeTransition { case AppState.QuestionDetails(prev, _) => prev }
                          )
                        )
                      ),
                      h2("Question"),
                      ul(
                        question.fragments.map(fragment => li(fragment.toString()))
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
}
