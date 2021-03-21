package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import levsha.events.EventPhase.AtTarget
import zio.logging.log
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.{ClientEvent, JsCallback}
import zodo.jeopardy.client.views.ViewState.PlayerState.{ChoosesQuestion, ThinkingAboutAnswer}
import zodo.jeopardy.client.views.ViewState._
import zodo.jeopardy.model.PackModel.Fragment.{Audio, Image, Text, Video}
import zodo.jeopardy.model.StageSnapshot._
import zodo.jeopardy.model.{PackMetaInfo, PackModel}

import scala.concurrent.duration.DurationInt
import scala.util.Random

class InGameView(val ctx: Context.Scope[AppTask, ViewState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(inGame: ViewState.InGame): DocumentNode = inGame match {
    case state @ ViewState.InGame(gameId, packMetaInfo, players, playerEvents, stage, countdown) =>
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
              case s: Round    => renderInRound(s, countdown, state.me.canAppeal)
              case s: Question =>
                renderQuestion(
                  packMetaInfo,
                  s.model,
                  countdown,
                  firstTime = true,
                  readyForHit = false,
                  canHit = true,
                  canAnswer = false
                )
              case s: ReadyForHit =>
                renderQuestion(
                  packMetaInfo,
                  s.model,
                  countdown,
                  firstTime = false,
                  readyForHit = true,
                  canHit = state.me.canHitButton,
                  canAnswer = false
                )
              case s: AnswerAttempt =>
                renderQuestion(
                  packMetaInfo,
                  s.model,
                  countdown,
                  firstTime = false,
                  readyForHit = false,
                  canHit = false,
                  canAnswer = state.me.canAnswer
                )
              case s: Answer       => renderInAnswer(packMetaInfo, s.model)
              case s: Appeal       => renderAppeal(s, state)
              case s: AppealResult => renderAppealResult(s.resolution)
            }
          )
        )
      }
  }

  private def renderCountdown(c: Option[Countdown]) = optimize {
    div(
      clazz := "game-zone__progress",
      c match {
        case Some(c) =>
          progress(
            clazz := "nes-progress",
            max := c.max.toString,
            value := c.remaining.toString
          )
        case None =>
          progress(
            clazz := "nes-progress",
            max := "1",
            value := "0"
          )
      }
    )
  }

  private def renderPlayer(playerEvents: PlayerEvents)(info: PlayerInfo): DocumentNode = optimize {
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
        ),
        if (state == ChoosesQuestion)
          a(
            `class` := "nes-badge",
            span(`class` := "is-primary", "A")
          )
        else void,
        playerEvents.appeal(id) match {
          case Some(AppealNotChosen) => i(`class` := "nes-icon is-medium")
          case Some(AppealAgree)     => i(`class` := "nes-icon is-medium like")
          case Some(AppealDisagree)  => i(`class` := "nes-icon is-medium like is-empty")
          case _                     => void
        }
      ),
      playerEvents.guess(id) match {
        case Some(g) if !g.isCorrect =>
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
        case _ => void
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

  private def renderInRound(round: Round, c: Option[Countdown], canAppeal: Boolean): DocumentNode = optimize {
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
      renderCountdown(c),
      div(
        `class` := "game-zone__controls",
        if (canAppeal) {
          button(
            `class` := "hit-button nes-btn is-warning",
            "APPEAL",
            event("click")(_.publish(ClientEvent.StartAppeal))
          )
        } else void
      )
    )
  }

  private def renderQuestion(
    packMetaInfo: PackMetaInfo,
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
                src := Urls.imageUrl(packMetaInfo, url)
              )
            )
          case Audio(url) =>
            div(
              `class` := "fragment audio-fragment",
              div("♪"),
              audio(
                AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                src := Urls.audioUrl(packMetaInfo, url),
                if (firstTime) autoplay := "autoplay" else void
              )
            )
          case Video(url) =>
            div(
              `class` := "fragment image-fragment",
              video(
                AttrDef("onEnded") := JsCallback.MediaFinished.call(question.id),
                src := Urls.videoUrl(packMetaInfo, url),
                if (firstTime) autoplay := "autoplay" else void
              )
            )
        }
      ),
      renderCountdown(c),
      div(
        `class` := "game-zone__controls",
        if (canAnswer) {
          renderAnswerInput
        } else {
          button(
            `class` := s"hit-button nes-btn " +
              s"${if (readyForHit && canHit) "is-error" else ""} " +
              s"${if (!canHit) "is-disabled" else ""}",
            autofocus := "true",
            "I KNOW",
            event("mousedown", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
            event("click", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
            event("touchstart", phase = AtTarget) { _.publish(ClientEvent.HitButton) }
          )
        }
      )
    )
  }

  private val answerInput = elementId()

  private def renderAnswerInput: DocumentNode = optimize {
    form(
      `class` := "answer-form",
      input(
        `class` := "answer-input nes-input",
        `type` := "text",
        answerInput
      ),
      button(
        `type` := "submit",
        `class` := "nes-btn",
        "OK"
      ),
      event("submit") { access =>
        for {
          answer <- access.valueOf(answerInput)
          _      <- access.publish(ClientEvent.GiveAnswer(answer))
        } yield ()
      }
    )
  }

  private def renderInAnswer(packMetaInfo: PackMetaInfo, answer: PackModel.Answers) = optimize {
    Seq(
      div(
        clazz := "game-zone__header",
        ""
      ),
      div(
        clazz := "game-zone__board",
        answer.correct.headOption match {
          case Some(text) =>
            div(
              `class` := "fragment",
              p(text)
            )
          case None => void
        },
        answer.additional.map {
          case Text(value) =>
            div(
              `class` := "fragment",
              p(value)
            )
          case Image(url) =>
            div(
              `class` := "fragment image-fragment",
              img(
                src := Urls.imageUrl(packMetaInfo, url)
              )
            )
          case Audio(url) =>
            div(
              `class` := "fragment audio-fragment",
              div("♪"),
              audio(
                src := Urls.audioUrl(packMetaInfo, url),
                autoplay := "autoplay"
              )
            )
          case Video(url) =>
            div(
              `class` := "fragment image-fragment",
              video(
                src := Urls.videoUrl(packMetaInfo, url),
                autoplay := "autoplay"
              )
            )
        }
      ),
      renderCountdown(None),
      div(`class` := "game-zone__controls")
    )
  }

  private def renderAppeal(s: Appeal, state: InGame) = optimize {
    Seq(
      div(
        clazz := "game-zone__header",
        s"${state.appealPlayerName.getOrElse("")} appeals"
      ),
      div(
        clazz := "game-zone__board",
        div(
          clazz := "fragment",
          s"Answer: ${s.answer}"
        ),
        div(
          clazz := "fragment",
          p(
            "Correct answers:"
          ),
          ul(s.model.answers.correct.map(li(_)))
        ),
        if (s.model.answers.incorrect.nonEmpty) {
          div(
            clazz := "fragment",
            p(
              "Incorrect answers:"
            ),
            ul(s.model.answers.incorrect.map(li(_)))
          )
        } else void
      ),
      renderCountdown(state.countdown),
      div(
        clazz := "game-zone__controls",
        button(
          if (!state.me.canVoteAppeal) disabled else void,
          clazz := s"hit-button nes-btn ${if (state.me.canVoteAppeal) "is-success" else "is-disabled"}",
          "AGREE",
          event("click")(_.publish(ClientEvent.ResolveAppeal(resolution = true)))
        ),
        button(
          if (!state.me.canVoteAppeal) disabled else void,
          clazz := s"hit-button nes-btn ${if (state.me.canVoteAppeal) "is-error" else "is-disabled"}",
          "DISAGREE",
          event("click")(_.publish(ClientEvent.ResolveAppeal(resolution = false)))
        )
      )
    )
  }

  private def renderAppealResult(resolution: Boolean) = optimize {
    Seq(
      div(clazz := "game-zone__header"),
      div(
        clazz := "game-zone__board",
        div(
          clazz := "fragment",
          s"Appeal ${if (resolution) "approved" else "not approved"}"
        )
      ),
      renderCountdown(None),
      div(clazz := "game-zone__controls")
    )
  }
}
