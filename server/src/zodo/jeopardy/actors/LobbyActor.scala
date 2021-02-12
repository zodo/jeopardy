package zodo.jeopardy.actors

import zio._
import zio.actors._
import zio.clock.Clock
import zio.logging._
import zio.random._
import zodo.jeopardy.model.{GameConfig, GameEntry, LobbyCommand, PlayerId}
import zodo.jeopardy.model.LobbyCommand._

object LobbyActor {

  case class State(
    entries: Map[String, GameEntry],
    players: Set[PlayerId]
  )
  object State {
    val init = State(Map(), Set())
  }

  type Env = Random with Clock with Logging with Has[GameConfig]

  val handler = new Actor.Stateful[Env, State, LobbyCommand] {
    override def receive[A](state: State, msg: LobbyCommand[A], context: Context): RIO[Env, (State, A)] =
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

        case AddPlayer(id) =>
          if (state.players.contains(id)) {
            UIO(state -> Left("Please close other tabs"))
          } else {
            UIO(state.copy(players = state.players + id) -> Right(()))
          }

        case RemovePlayer(id) => UIO(state.copy(players = state.players - id) -> ())
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
