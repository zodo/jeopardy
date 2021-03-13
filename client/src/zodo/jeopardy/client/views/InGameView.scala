package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import levsha.dsl.html.div
import levsha.events.EventPhase.AtTarget
import zio.logging.log
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.{ClientEvent, JsCallback}
import zodo.jeopardy.client.views.ViewState.PlayerState.ThinkingAboutAnswer
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
          `class` := "container",
          div(
            `class` := "player-list",
            players
              .map(renderPlayer(playerEvents))
          ),
          div(
            `class` := "game-zone",
            stage match {
              case BeforeStart => renderWaitingForStart(gameId)
              case s: Round    => renderInRound(s, countdown)
              case s: Question =>
                renderQuestion(
                  hash,
                  s.model,
                  countdown,
                  firstTime = true,
                  readyForHit = false,
                  canHit = true,
                  canAnswer = false
                )
              case s: ReadyForHit =>
                renderQuestion(
                  hash,
                  s.model,
                  countdown,
                  firstTime = false,
                  readyForHit = true,
                  canHit = state.me.canHitButton,
                  canAnswer = false
                )
              case s: AnswerAttempt =>
                renderQuestion(
                  hash,
                  s.model,
                  countdown,
                  firstTime = false,
                  readyForHit = false,
                  canHit = false,
                  canAnswer = state.me.canAnswer
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
        )
      }
  }

  private def renderCountdown(c: Option[Countdown]) = optimize {
    c match {
      case Some(c) =>
        div(
          clazz := "game-zone__progress",
          progress(
            clazz := "nes-progress",
            max := c.max.toString,
            value := c.remaining.toString
          )
        )
      case None => void
    }
  }

  private def renderPlayer(playerEvents: PlayerEvents)(info: PlayerInfo): DocumentNode = {
    val PlayerInfo(id, name, score, state, me, disconnected) = info

    val answering = if (state == ThinkingAboutAnswer) "player__answering" else ""
    val hitButton = if (playerEvents.isButtonPressed(id)) "player__hit-button" else ""
    val dscnnctd = if (disconnected) "player__disconnected" else ""

    div(
      `class` := s"player $answering $hitButton $dscnnctd",
      div(
        `class` := "player__content",
        p(
          `class` := "player__name",
          title := name,
          name + (if (me) " (me)" else "")
        ),
        p(
          title := id,
          score.toString
        )
      ),
      playerEvents.guess(id) match {
        case Some(g) =>
          span(
            clazz := "guess",
            div(
              clazz := "guess-popup",
              div(
                clazz := "nes-balloon from-left",
                p(g.answer)
              )
            )
          )
        case None => void
      },
      playerEvents.appeal(id) match {
        case Some(AppealInitiated) => div("Appeal!")
        case Some(AppealAgree)     => div("I'm OK with appeal")
        case Some(AppealDisagree)  => div("I'm not OK with appeal")
        case None                  => void
      }
    )
  }

  private def renderWaitingForStart(gameId: String): DocumentNode = optimize {
    Seq(
      div(
        clazz := "game-zone__wait center-container",
        div(
          div(
            p(
              "Game #",
              code(gameId)
            )
          ),
          form(
            button(
              `type` := "submit",
              clazz := "nes-btn is-success",
              "Start now"
            ),
            event("submit")(_.publish(ClientEvent.StartGame))
          )
        )
      )
    )
  }

  private def renderInRound(round: Round, c: Option[Countdown]): DocumentNode = {
    Seq(
      div(
        clazz := "game-zone__header",
        round.model.name
      ),
      div(
        clazz := "game-zone__board",
        div(
          `class` := "fragment",
          tbody(
            clazz := "nes-table is-bordered is-centered",
            round.model.themes.map(theme =>
              tr(
                td(theme.name),
                theme.questions.map(question =>
                  td(
                    when(round.takenQuestions.contains(question.id))(clazz := "table__taken-question"),
                    question.price.toString(),
                    event("click")(_.publish(ClientEvent.SelectQuestion(question.id)))
                  )
                )
              )
            )
          )
        )
      ),
      renderCountdown(c)
    )
  }

  private def renderQuestion(
    hash: String,
    question: PackModel.Question,
    c: Option[Countdown],
    firstTime: Boolean,
    readyForHit: Boolean,
    canHit: Boolean,
    canAnswer: Boolean
  ): DocumentNode = optimize {
    Seq(
      div(
        clazz := "game-zone__header",
        s"${question.theme} - ${question.price}"
      ),
      div(
        clazz := "game-zone__board",
        question.fragments.map {
          case Text(value) =>
            div(
              `class` := "fragment",
              delay((value.length / 15 + 3).seconds) { access =>
                log.debug("Text ended!") *>
                  access.publish(ClientEvent.FinishQuestionReading(question.id))
              },
              p(value)
            )
          case Image(url) =>
            div(
              `class` := "fragment image-fragment",
              img(
                delay((Random.nextInt(5) + 3).seconds) { access =>
                  log.debug("Image ended!") *>
                    access.publish(ClientEvent.FinishQuestionReading(question.id))
                },
                src := s"/media/$hash/Images/${url.drop(1)}"
              )
            )
          case Audio(url) =>
            div(
              `class` := "fragment audio-fragment",
              div("♪"),
              audio(
                AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                src := s"/media/$hash/Audio/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                if (firstTime) autoplay := "autoplay" else void
              )
            )
          case Video(url) =>
            div(
              `class` := "fragment image-fragment",
              video(
                AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                src := s"/media/$hash/Video/${URLEncoder.encode(url.drop(1), "UTF-8")}",
                if (firstTime) autoplay := "autoplay" else void
              )
            )
        }
      ),
      renderCountdown(c),
      div(
        `class` := "game-zone__controls",
        button(
          if (!canHit) disabled else void,
          backgroundColor @= (if (readyForHit) "red" else "green"),
          autofocus := "true",
          "I know!",
          event("mousedown", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("click", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("touchstart", phase = AtTarget) { _.publish(ClientEvent.HitButton) }
        ),
        if (canAnswer) renderAnswerInput else void
      )
    )
  }

  private val answerInput = elementId()

  private def renderAnswerInput: DocumentNode = optimize {
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
