package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import ViewState._
import levsha.events.EventPhase.AtTarget
import zodo.jeopardy.actors.GameActor.OutgoingMessage.SimpleStage._
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.Image

class InGameView(val ctx: Context.Scope[AppTask, ViewState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(s: ViewState.InGame): DocumentNode = s match {
    case ViewState.InGame(gameId, hash, players, stage) =>
      optimize {
        div(
          h2(s"In game '$gameId' with pack $hash"),
          players
            .map(renderPlayer),
          stage match {
            case WaitingForStart     => renderWaitingForStart
            case s: InRound          => renderInRound(s.round, s.takenQuestions)
            case s: InQuestion       => renderInQuestion(hash, s.question)
            case s: InAwaitingAnswer => renderInQuestion(hash, s.question)
          }
        )
      }
  }

  private def renderPlayer(info: PlayerInfo) = {
    val PlayerInfo(id, name, score, state, me, buttonPressed) = info

    div(
      ul(
        h3(
          if (buttonPressed) backgroundColor @= "red" else void,
          s"$name${if (me) "(its me!)" else ""} - $state"
        ),
        li(s"Id - $id"),
        li(s"Score - $score")
      )
    )
  }

  private def renderWaitingForStart: DocumentNode = {
    div(
      h2("Waiting for start"),
      button(
        "start now!",
        event("click")(_.publish(ClientEvent.StartGame))
      )
    )
  }

  private def renderInRound(round: PackModel.Round, takenQuestions: Set[String]): DocumentNode = {
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

  private def renderInQuestion(hash: String, question: PackModel.Question): DocumentNode = {
    div(
      h2("Question"),
      question.fragments.map(fragment =>
        div(
          fragment match {
            case Image(url) =>
              img(
                width @= "600px",
                src := s"/media/$hash/Images/${url.drop(1)}"
              )
            case _ => fragment.toString
          }
        )
      ),
      button(
        backgroundColor @= "green",
        autofocus := "true",
        "I know!",
        event("mousedown", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
        event("click", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
        event("touchstart", phase = AtTarget) { _.publish(ClientEvent.HitButton) }
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
