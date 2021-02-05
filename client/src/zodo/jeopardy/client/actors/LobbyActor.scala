package zodo.jeopardy.client.actors

import zio.{RIO, UIO, URIO, ZIO}
import zio.actors.Actor.Stateful
import zio.actors.{ActorRef, Context, Supervisor}
import zio.clock.Clock
import zio.random._
import zio.logging._
import zodo.jeopardy.client.actors.GameActor.GameActorRef
import zodo.jeopardy.client.actors.LobbyActor.Message.{EndGame, GetGame, NewGame}
import zodo.jeopardy.model.PackModel

object LobbyActor {

  type LobbyActorRef = ActorRef[Message]

  sealed trait Message[+_]

  object Message {
    case class NewGame(hash: String, pack: PackModel.Pack) extends Message[(String, GameActorRef)]
    case class GetGame(id: String) extends Message[Option[GameActorRef]]
    case class EndGame(id: String) extends Message[Unit]
  }

  case class State(games: Map[String, GameActorRef])

  object State {
    val init = State(Map())
  }

  type Env = Random with Clock with Logging

  val handler = new Stateful[Env, State, Message] {

    override def receive[A](state: State, msg: Message[A], context: Context): RIO[Env, (State, A)] =
      msg match {
        case NewGame(hash, pack) =>
          for {
            _ <- log.debug(s"LobbyActor <- NewGame($hash)")
            id <- randomGameId
            gameActor <- context.make(s"game-$id", Supervisor.none, GameActor.initState(pack), GameActor.handler)
            pair = id -> gameActor
          } yield state.copy(games = state.games + pair) -> pair.asInstanceOf[A]
        case GetGame(id) =>
          log
            .debug(s"LobbyActor <- GetGame($id)")
            .as(state -> state.games.get(id).asInstanceOf[A])
        case EndGame(id) =>
          (for {
            game <- ZIO.fromOption(state.games.get(id))
            _ <- game.stop
          } yield ()).ignore.as(state.copy(games = state.games.removed(id)) -> ().asInstanceOf[A])
      }
  }

  private val randomGameId: URIO[Random, String] = {
    val symbols = ('a' to 'z').map(_.toString)
    val mask = "###-###-###"
    val maskSymbol = "#"

    val randomPossibleChar = nextIntBounded(symbols.length).map(symbols(_))

    def aux(mask: String): ZIO[Random, Nothing, String] = {
      if (!mask.contains(maskSymbol)) UIO(mask)
      else
        for {
          c <- randomPossibleChar
          ret <- aux(mask.replaceFirst(maskSymbol, c))
        } yield ret
    }

    aux(mask)
  }
}
