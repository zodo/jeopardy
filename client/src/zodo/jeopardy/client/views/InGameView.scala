package zodo.jeopardy.client.views

import korolev.Context
import korolev.effect.Effect
import ViewState._
import levsha.events.EventPhase.AtTarget
import zio.logging.log
import zodo.jeopardy.model.StageSnapshot._
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.Image

import scala.concurrent.duration._

class InGameView(val ctx: Context.Scope[AppTask, ViewState, InGame, ClientEvent])(implicit eff: Effect[AppTask]) {

  import ctx._
  import levsha.dsl._
  import html._

  def render(inGame: ViewState.InGame): DocumentNode = inGame match {
    case ViewState.InGame(gameId, hash, players, stage) =>
      optimize {
        div(
          h2(s"In game '$gameId' with pack $hash"),
          players
            .map(renderPlayer),
          stage match {
            case BeforeStart => renderWaitingForStart
            case s: Round    => renderInRound(s.round, s.takenQuestions)
            case s: Question => renderQuestion(hash, s.question)
            case s: AnswerAttempt =>
              div(
                renderQuestion(hash, s.question),
                if (inGame.me.exists(_.id == s.activePlayer)) renderAnswerInput else void
              )
            case s: Answer => renderInAnswer(hash, s.answer)
          }
        )
      }
  }

  private def renderPlayer(info: PlayerInfo): DocumentNode = {
    val PlayerInfo(id, name, score, state, me, buttonPressed, guess) = info

    div(
      title := s"Id - $id",
      ul(
        h3(
          if (buttonPressed) {
            delay(1.seconds)(access =>
              for {
                _ <- log.debug(s"Transitioning $name to false")
                _ <- access.transition(_.withPlayers(_.id == id, _.copy(buttonPressed = false)))
              } yield ()
            )
          } else void,
          when(buttonPressed)(backgroundColor @= "red"),
          s"$name${if (me) "(its me!)" else ""} - $state"
        ),
        li(s"Score - $score"),
        guess match {
          case Some(g) =>
            li(
              delay(5.seconds)(_.transition(_.withPlayers(_.id == id, _.copy(guess = None)))),
              s"Guess - $g"
            )
          case None => void
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

  private def renderQuestion(hash: String, question: PackModel.Question): DocumentNode = {
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
      div(
        button(
          backgroundColor @= "green",
          autofocus := "true",
          "I know!",
          event("mousedown", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("click", phase = AtTarget) { _.publish(ClientEvent.HitButton) },
          event("touchstart", phase = AtTarget) { _.publish(ClientEvent.HitButton) }
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
