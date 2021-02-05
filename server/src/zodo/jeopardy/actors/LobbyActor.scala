package zodo.jeopardy.actors

import zio._
import zio.actors._
import zio.logging._
import zio.clock.Clock
import zio.random._
import zodo.jeopardy.actors.GameActor.GameActorRef
import zodo.jeopardy.actors.LobbyActor.Message.{EndGame, GetGameEntry, NewGame}
import zodo.jeopardy.actors.LobbyActor.State.GameEntry
import zodo.jeopardy.model.PackModel

object LobbyActor {

  type LobbyActorRef = ActorRef[Message]

  sealed trait Message[+_]
  object Message {
    case class NewGame(hash: String, pack: PackModel.Pack) extends Message[GameEntry]
    case class GetGameEntry(id: String) extends Message[Option[GameEntry]]
    case class EndGame(id: String) extends Message[Unit]
  }

  case class State(entries: Map[String, GameEntry])
  object State {
    case class GameEntry(id: String, packId: String, game: GameActorRef)
    val init = State(Map())
  }

  type Env = Random with Clock with Logging

  val handler = new Actor.Stateful[Env, State, Message] {
    override def receive[A](state: State, msg: Message[A], context: Context): RIO[Env, (State, A)] =
      msg match {
        case NewGame(hash, pack) =>
          for {
            _  <- log.debug(s"LobbyActor <- NewGame($hash)")
            id <- randomGameId
            gameActor <- context.make(
              s"game-$id",
              actors.Supervisor.none,
              GameActor.initState(pack),
              GameActor.handler
            )
            entry = GameEntry(id, hash, gameActor)
          } yield state.copy(entries = state.entries.updated(id, entry)) -> entry
        case GetGameEntry(id) =>
          log
            .debug(s"LobbyActor <- GetGame($id)")
            .as(state -> state.entries.get(id))
        case EndGame(id) =>
          (for {
            entry <- ZIO.fromOption(state.entries.get(id))
            _     <- entry.game.stop
          } yield ()).ignore.as(state.copy(entries = state.entries.removed(id)) -> ())
      }
  }

  private val randomGameId: URIO[Random, String] = {
    val symbols = ('a' to 'z').map(_.toString)
    val mask = "##"
    val maskSymbol = "#"

    val randomPossibleChar = nextIntBounded(symbols.length).map(symbols(_))

    def aux(mask: String): ZIO[Random, Nothing, String] = {
      if (!mask.contains(maskSymbol)) UIO(mask)
      else
        for {
          c   <- randomPossibleChar
          ret <- aux(mask.replaceFirst(maskSymbol, c))
        } yield ret
    }

    aux(mask)
  }
}
