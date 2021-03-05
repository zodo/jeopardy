package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import levsha.events.EventPhase
import levsha.events.EventPhase.AtTarget
import zio.logging.log
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.{ClientEvent, JsCallback}
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.{PackModel, PlayerId}
import zodo.jeopardy.model.PackModel.Fragment.{Audio, Image, Text, Video}
import zodo.jeopardy.model.StageSnapshot._

import java.net.URLEncoder
import scala.concurrent.duration.DurationInt
import scala.util.Random

class InGameView(val ctx: Context.Scope[AppTask, ViewState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(inGame: ViewState.InGame): DocumentNode = inGame match {
    case ViewState.InGame(gameId, hash, players, playerEvents, stage, countdown) =>
      optimize {
        div(
          h2(s"In game '$gameId' with pack $hash"),
          players
            .map(renderPlayer(playerEvents)),
          countdown match {
            case Some(c) =>
              progress(
                max := c.max.toString,
                value := c.remaining.toString
              )
            case None => void
          },
          stage match {
            case BeforeStart    => renderWaitingForStart
            case s: Round       => renderInRound(s.model, s.takenQuestions)
            case s: Question    => renderQuestion(hash, s.model, firstTime = true, readyForHit = false)
            case s: ReadyForHit => renderQuestion(hash, s.model, firstTime = false, readyForHit = true)
            case s: AnswerAttempt =>
              div(
                renderQuestion(hash, s.model, firstTime = false, readyForHit = false),
                if (inGame.me.exists(_.id == s.activePlayer)) renderAnswerInput else void
              )
            case s: Answer => renderInAnswer(hash, s.model)
          }
        )
      }
  }

  private def renderPlayer(playerEvents: PlayerEvents)(info: PlayerInfo): DocumentNode = {
    val PlayerInfo(id, name, score, state, me, disconnected) = info

    div(
      when(disconnected)(color @= "gray"),
      title := s"Id - $id",
      ul(
        h3(
          when(playerEvents.isButtonPressed(id))(backgroundColor @= "red"),
          s"$name${if (me) "(its me!)" else ""} - $state"
        ),
        li(s"Score - $score"),
        playerEvents.guess(id) match {
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

  private def renderQuestion(
    hash: String,
    question: PackModel.Question,
    firstTime: Boolean,
    readyForHit: Boolean
  ): DocumentNode = {
    div(
      h2("Question"),
      div(
        button(
          backgroundColor @= (if (readyForHit) "red" else "green"),
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
            case Text(value) =>
              h1(
                delay((value.length / 15 + 3).seconds) { access =>
                  log.debug("Text ended!") *>
                    access.publish(ClientEvent.FinishQuestionReading(question.id))
                },
                value
              )
            case Image(url) =>
              img(
                delay((Random.nextInt(5) + 3).seconds) { access =>
                  log.debug("Image ended!") *>
                    access.publish(ClientEvent.FinishQuestionReading(question.id))
                },
                width @= "600px",
                src := s"/media/$hash/Images/${url.drop(1)}"
              )
            case Audio(url) =>
              div(
                p("AUDIO"),
                audio(
                  AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                  src := s"/media/$hash/Audio/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                  if (firstTime) autoplay := "autoplay" else void
                )
              )

            case Video(url) =>
              video(
                AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                src := s"/media/$hash/Video/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                if (firstTime) autoplay := "autoplay" else void
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
