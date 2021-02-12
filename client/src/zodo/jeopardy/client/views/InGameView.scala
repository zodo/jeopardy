package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import levsha.events.EventPhase.AtTarget
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.{Audio, Image, Text, Video}
import zodo.jeopardy.model.StageSnapshot._

import java.net.URLEncoder

class InGameView(val ctx: Context.Scope[AppTask, ViewState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(inGame: ViewState.InGame): DocumentNode = inGame match {
    case ViewState.InGame(gameId, hash, players, stage, countdown) =>
      optimize {
        div(
          h2(s"In game '$gameId' with pack $hash"),
          players
            .map(renderPlayer),
          countdown match {
            case Some(c) =>
              progress(
                max := c.max.toString,
                value := c.remaining.toString
              )
            case None => void
          },
          stage match {
            case BeforeStart => renderWaitingForStart
            case s: Round    => renderInRound(s.round, s.takenQuestions)
            case s: Question => renderQuestion(hash, s.question, countdown)
            case s: AnswerAttempt =>
              div(
                renderQuestion(hash, s.question, countdown),
                if (inGame.me.exists(_.id == s.activePlayer)) renderAnswerInput else void
              )
            case s: Answer => renderInAnswer(hash, s.answer)
          }
        )
      }
  }

  private def renderPlayer(info: PlayerInfo): DocumentNode = {
    val PlayerInfo(id, name, score, state, me, buttonPressed, guess, disconnected) = info

    div(
      when(disconnected)(color @= "gray"),
      title := s"Id - $id",
      ul(
        h3(
          when(buttonPressed)(backgroundColor @= "red"),
          s"$name${if (me) "(its me!)" else ""} - $state"
        ),
        li(s"Score - $score"),
        guess match {
          case Some(g) => li(s"Guess - $g")
          case None    => void
        }
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
                when(takenQuestions.contains(question.id))(color @= "gray"),
                question.price.toString(),
                event("click")(_.publish(ClientEvent.SelectQuestion(question.id)))
              )
            )
          )
        )
      )
    )
  }

  private def renderQuestion(hash: String, question: PackModel.Question, countdown: Option[Countdown]): DocumentNode = {
    div(
      h2("Question"),
      div(
        button(
          backgroundColor @= "green",
          autofocus := "true",
          "I know!",
          event("mousedown", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("click", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("touchstart", phase = AtTarget) { _.publish(ClientEvent.HitButton) }
        )
      ),
      question.fragments.map(fragment =>
        div(
          fragment match {
            case Text(value) => h1(value)
            case Image(url) =>
              img(
                width @= "600px",
                src := s"/media/$hash/Images/${url.drop(1)}"
              )
            case Audio(url) =>
              audio(
                src := s"/media/$hash/Audio/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                autoplay := "true"
              )
            case Video(url) =>
              video(
                src := s"/media/$hash/Video/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                autoplay := "true"
              )
          }
        )
      )
    )
  }

  private val answerInput = elementId()

  private def renderAnswerInput: DocumentNode = {
    div(
      form(
        input(
          `type` := "text",
          answerInput
        ),
        button(
          "Submit answer"
        ),
        event("submit") { access =>
          for {
            answer <- access.valueOf(answerInput)
            _      <- access.publish(ClientEvent.GiveAnswer(answer))
          } yield ()
        }
      )
    )
  }

  private def renderInAnswer(hash: String, answer: PackModel.Answers) = {
    div(
      h2(answer.correct.head)
    )
  }
}
