package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import ViewState._
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.client.views.ViewState.GameState._
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.Image

class InGameView(val ctx: Context.Scope[AppTask, RootState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(s: ViewState.InGame) = s match {
    case ViewState.InGame(gi @ GameInfo(gameId, hash, players), gameState) =>
      optimize {
        div(
          h2(s"In game $gameId with pack $hash"),
          players
            .map(renderPlayer),
          gameState match {
            case WaitingForStart                => renderWaitingForStart(gameId)
            case InRound(round, takenQuestions) => renderInRound(round, takenQuestions)
            case InQuestion(question)           => renderInQuestion(hash, question)
            case InAnswer(answer)               => renderInAnswer(answer)
          },
          input(
            `type` := "text",
            if (gi.me.exists(_.state != PlayerState.Answer)) disabled else void
          ),
          button(
            if (gi.me.exists(_.state != PlayerState.Answer)) disabled else void,
            "Send"
          )
        )
      }
  }

  private def renderPlayer(info: PlayerInfo) = {
    val PlayerInfo(id, name, score, state, me) = info

    div(
      ul(
        h3(s"Player${if (me) "(its me!)" else ""}"),
        li(s"Id - $id"),
        li(s"Name - $name"),
        li(s"Score - $score"),
        li(s"State - $state")
      )
    )
  }

  private def renderWaitingForStart(gameId: String) = {
    div(
      h2("Waiting for start"),
      button(
        "start now!",
        event("click")(_.publish(ClientEvent.StartGame(gameId)))
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
            td(theme.name),
            theme.questions.map(question =>
              td(
                if (takenQuestions.contains(question.id))
                  color @= "gray"
                else void,
                question.price.toString(),
                event("click")(_.publish(ClientEvent.ChooseQuestion(question.id)))
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
