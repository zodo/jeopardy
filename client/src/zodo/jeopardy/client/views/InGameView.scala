package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import levsha.events.EventPhase.AtTarget
import zio.logging.log
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.{ClientEvent, JsCallback}
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.PackModel
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
    case state @ ViewState.InGame(gameId, hash, players, playerEvents, stage, countdown) =>
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
            case BeforeStart => renderWaitingForStart
            case s: Round    => renderInRound(s.model, s.takenQuestions)
            case s: Question =>
              renderQuestion(hash, s.model, firstTime = true, readyForHit = false, canHit = true)
            case s: ReadyForHit =>
              renderQuestion(hash, s.model, firstTime = false, readyForHit = true, canHit = state.me.canHitButton)
            case s: AnswerAttempt =>
              div(
                renderQuestion(hash, s.model, firstTime = false, readyForHit = false, canHit = false),
                if (state.me.canAnswer) renderAnswerInput else void
              )
            case s: Answer       => renderInAnswer(hash, s.model)
            case s: Appeal       => renderAppeal(s.answer, s.model, state.me.canVoteAppeal)
            case s: AppealResult => renderAppealResult(s.resolution)
          },
          if (state.me.canAppeal) {
            button(
              "Appeal!",
              event("click")(_.publish(ClientEvent.StartAppeal))
            )
          } else void
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
        },
        playerEvents.appeal(id) match {
          case Some(AppealInitiated) => li("Appeal!")
          case Some(AppealAgree)     => li("I'm OK with appeal")
          case Some(AppealDisagree)  => li("I'm not OK with appeal")
          case None                  => void
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
    readyForHit: Boolean,
    canHit: Boolean
  ): DocumentNode = {
    div(
      h2("Question"),
      div(
        button(
          if (!canHit) disabled else void,
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

  private def renderAppeal(answer: String, model: PackModel.Question, canVote: Boolean) = {
    div(
      h2("Appeal"),
      div("Question: "),
      model.fragments.collect {
        case Text(value) => div(value)
      },
      div(s"Answers was: $answer"),
      div(s"Correct answers: "),
      ul(
        model.answers.correct.map(li(_))
      ),
      if (model.answers.incorrect.nonEmpty) {
        div(
          "incorrect answers",
          ul(
            model.answers.incorrect.map(li(_))
          )
        )
      } else void,
      if (canVote) {
        div(
          button(
            "Agree",
            event("click")(_.publish(ClientEvent.ResolveAppeal(resolution = true)))
          ),
          button(
            "Disagree",
            event("click")(_.publish(ClientEvent.ResolveAppeal(resolution = false)))
          )
        )
      } else void
    )
  }

  private def renderAppealResult(resolution: Boolean) = {
    h2(s"Appeal ${if (resolution) "approved" else "not approved"}")
  }
}
