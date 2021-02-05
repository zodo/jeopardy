package zodo.jeopardy.client

import korolev.Context
import korolev.effect.Effect
import korolev.server.{KorolevServiceConfig, StateLoader}
import korolev.state.javaSerialization._
import zio.{RIO, ZIO, actors}
import zio.actors.Actor.Stateful
import zio.actors.{ActorRef, Supervisor}
import zodo.jeopardy.client.AppState._
import zodo.jeopardy.client.actors.LobbyActor
import zodo.jeopardy.client.actors.LobbyActor.LobbyActorRef
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.model.PackModel.Fragment.Image

import scala.concurrent.ExecutionContext

class KorolevService(implicit eff: Effect[AppTask], ec: ExecutionContext) {

  val configM = for {
    system <- DefaultActorSystem.system
    lobby <- system.make("lobby", Supervisor.none, LobbyActor.State.init, LobbyActor.handler)
  } yield config(lobby)

  val ctx = Context[AppTask, AppState, ClientEvent]

  import ctx._
  import levsha.dsl._
  import html._

  private def config(lobby: LobbyActorRef) = {

    val eventsMediator = new EventsMediator(lobby, ctx)
    val uploader = new Uploader
    val nameInputId = elementId()
    val gameInputId = elementId()

    KorolevServiceConfig[AppTask, AppState, ClientEvent](
      stateLoader = StateLoader.default(AppState.Anonymous),
      extensions = List(eventsMediator),
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
                    form(
                      input(nameInputId, `type` := "text", placeholder := "Your name"),
                      button("OK"),
                      event("submit")(access =>
                        for {
                          name <- access.valueOf(nameInputId)
                          _ <- access.publish(ClientEvent.Introduce(name))
                        } yield ()
                      )
                    )

                  case AppState.Authorized(name, error) =>
                    div(
                      h2(s"Hello $name!"),
                      div(
                        h3("Either create a new game"),
                        uploader(()) { (access, event) => access.publish(event) },
                        h3("Or join the existing"),
                        form(
                          input(gameInputId, `type` := "text", placeholder := "Game ID"),
                          button("Join"),
                          event("submit")(access =>
                            for {
                              gameId <- access.valueOf(gameInputId)
                              _ <- access.publish(ClientEvent.EnterGame(gameId))
                            } yield ()
                          )
                        ),
                        error match {
                          case Some(msg) =>
                            p(
                              backgroundColor @= "red",
                              msg
                            )
                          case None => void
                        }
                      )
                    )

                  case AppState.InGame(gi @ GameInfo(gameId, hash, players), gameState) =>
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
                  case _ => div("Something is wrong")
                }
              )
            )
          )
        }
    )
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
            style := "cursor: pointer",
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
