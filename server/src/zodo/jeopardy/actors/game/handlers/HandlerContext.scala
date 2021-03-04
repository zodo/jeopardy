package zodo.jeopardy.actors.game.handlers
import zio.actors.ActorRef
import zio.duration.durationInt
import zio.logging.{Logging, log}
import zio.{RIO, URIO, ZIO}
import zodo.jeopardy.actors.game.State.Countdown
import zodo.jeopardy.actors.game.{CountdownId, Env, State}
import zodo.jeopardy.actors.{GameActorRef, LobbyActorRef}
import zodo.jeopardy.model.GameEvent.{CountdownModel, CountdownUpdated}
import zodo.jeopardy.model.{GameCommand, GameConfig, GameEvent}

import java.util.UUID

trait HandlerContext {
  val lobby: LobbyActorRef
  val config: GameConfig
  val self: GameActorRef

  def broadcast(m: GameEvent[Unit]): RIO[Logging, Unit]
  def broadcast(m: GameEvent[Unit], useState: State): RIO[Logging, Unit]
  def setCountdown(seconds: Int, startsFrom: Int = 1)(
    afterCountdown: GameActorRef => RIO[Env, Unit]
  ): RIO[Env, (CountdownId, Countdown)]
  def stoppedCountdown(id: CountdownId): URIO[Logging, Option[Countdown]]
}

object HandlerContext {
  class Default(
    val lobby: LobbyActorRef,
    val config: GameConfig,
    val self: GameActorRef,
    state: State
  ) extends HandlerContext {

    override def broadcast(m: GameEvent[Unit]) = broadcast(m, state)

    override def broadcast(m: GameEvent[Unit], useState: State) =
      ZIO
        .foreachPar(useState.players.filterNot(_.disconnected))(p => log.debug(s"Sending $m to $p") *> (p.reply ! m))
        .unit

    override def setCountdown(seconds: Int, startsFrom: Int = 1)(
      afterCountdown: ActorRef[GameCommand] => RIO[Env, Unit]
    ): RIO[Env, (CountdownId, Countdown)] = {
      val id = UUID.randomUUID().toString

      def inBackground = ZIO
        .foreach_(startsFrom to seconds) { tick =>
          for {
            _ <- ZIO.sleep(1.second)
            _ <- self ! GameCommand.TickCountdown(tick, id)
          } yield ()
        }

      for {
        _     <- log.debug(s"set countdown for $seconds with id $id")
        _     <- broadcast(CountdownUpdated(Some(CountdownModel(startsFrom - 1, seconds))))
        fiber <- inBackground.fork
      } yield id -> Countdown(seconds, 0, fiber, afterCountdown(self))
    }
    override def stoppedCountdown(id: CountdownId): URIO[Logging, Option[Countdown]] = {
      (for {
        _  <- log.debug(s"stop countdown $id")
        cd <- ZIO.fromOption(state.countdowns.get(id))
        _  <- cd.fiber.interrupt
        _  <- broadcast(GameEvent.CountdownUpdated(None))
        _  <- log.debug(s"countdown $id stopped")
      } yield cd).option
    }
  }
}
