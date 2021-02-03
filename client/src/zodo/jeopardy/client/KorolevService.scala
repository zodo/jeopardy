package zodo.jeopardy.client

import zio.console._
import korolev.Context
import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import zodo.jeopardy.client.AppEvent.FileProcessed
import zodo.jeopardy.client.AppState.{GameInfo, InAnswer, InQuestion, InRound, PlayerInfo}
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
    stateLoader = StateLoader.default(AppState.Anonymous),
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
                case AppState.Anonymous =>
                  uploader(()) { (access, event) =>
                    event match {
                      case FileProcessed(hash, pack) =>
                        putStrLn(s"File with $hash uploaded succesfully")
                    }
                  }
                case AppState.InGame(gi @ GameInfo(gameId, hash, players), gameState) =>
                  div(
                    h2(s"In game $gameId with pack $hash"),
                    players.map(renderPlayer),
                    gameState match {
                      case InRound(round, takenQuestions) => renderInRound(round, takenQuestions)
                      case InQuestion(question)           => renderInQuestion(hash, question)
                      case InAnswer(answer)               => renderInAnswer(answer)
                    },
                    input(
                      `type` := "text",
                      if (!gi.me.givingAnswer) disabled else void
                    ),
                    button(
                      if (!gi.me.givingAnswer) disabled else void,
                      "Send"
                    )
                  )
              }
            )
          )
        )
      }
  )

  private def renderPlayer(info: PlayerInfo) = {
    val PlayerInfo(id, name, score, givingAnswer, me) = info

    div(
      ul(
        h3(s"Player${if (me) "(its me!)" else ""}"),
        li(s"Id - $id"),
        li(s"Name - $name"),
        li(s"Score - $score"),
        li(s"Giving answer - $givingAnswer")
      )
    )
  }

  private def renderInRound(round: PackModel.Round, takenQuestions: Set[String]) = {
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
                if (takenQuestions.contains(question.id))
                  color @= "gray"
                else void,
                question.price.toString()
              )
            )
          )
        )
      )
    )
  }

  private def renderInQuestion(hash: String, question: PackModel.Question) = {
    div(
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
      )
    )
  }

  private def renderInAnswer(answer: PackModel.Answers) = {
    div(
      h2("Answer"),
      ul(answer.correct.map(a => li(a)))
    )
  }

}
