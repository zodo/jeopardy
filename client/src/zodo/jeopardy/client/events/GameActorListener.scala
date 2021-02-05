package zodo.jeopardy.client.events

import zio._
import zio.actors._
import zio.logging._
import zodo.jeopardy.actors.GameActor
import zodo.jeopardy.actors.GameActor.OutgoingMessage.{NewPlayerConnected, RoundStarted}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.RootState
import zodo.jeopardy.client.views.ViewState.GameState.InRound
import zodo.jeopardy.client.views.ViewState._

object GameActorListener {

  def handler(access: Access): Actor.Stateful[AppEnv, Unit, GameActor.OutgoingMessage] =
    new Actor.Stateful[AppEnv, Unit, GameActor.OutgoingMessage] {

      override def receive[A](
        state: Unit,
        msg: GameActor.OutgoingMessage[A],
        context: actors.Context
      ): RIO[AppEnv, (Unit, A)] = msg match {
        case NewPlayerConnected(id, name) =>
          for {
            _       <- log.debug(s"GameActorListener <- NewPlayerConnected($id, $name)")
            session <- access.sessionId
            _ <- access.maybeTransition { case r @ RootState(_, s @ InGame(GameInfo(_, _, players), _)) =>
              r.complete(
                s.copy(gameInfo =
                  s.gameInfo.copy(players =
                    players :+ PlayerInfo(
                      id,
                      name,
                      0,
                      PlayerState.Idle,
                      session.toString == id
                    )
                  )
                )
              )
            }
          } yield () -> ()
        case RoundStarted(round) =>
          for {
            _ <- log.debug(s"GameActorListener <- RoundStarted")
            _ <- access
              .maybeTransition { case r @ RootState(_, s: InGame) =>
                r.complete(s.copy(gameState = InRound(round, Set())))
              }
          } yield () -> ()
      }
    }
}
