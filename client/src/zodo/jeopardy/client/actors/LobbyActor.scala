package zodo.jeopardy.client.actors

import zio.{RIO, UIO, URIO, ZIO}
import zio.actors.Actor.Stateful
import zio.actors.{ActorRef, Context, Supervisor}
import zio.clock.Clock
import zio.random._
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

  val handler = new Stateful[Random with Clock, State, Message] {

    override def receive[A](state: State, msg: Message[A], context: Context): RIO[Random with Clock, (State, A)] =
      msg match {
        case NewGame(hash, pack) =>
          println(s"LobbyActor <- NewGame($hash)")
          for {
            id <- randomGameId
            gameActor <- context.make(s"game-$id", Supervisor.none, GameActor.initState(pack), GameActor.handler)
            pair = id -> gameActor
          } yield state.copy(games = state.games + pair) -> pair.asInstanceOf[A]
        case GetGame(id) =>
          println(s"LobbyActor <- GetGame($id)")
          UIO(state -> state.games.get(id).asInstanceOf[A])
        case EndGame(id) =>
          (for {
            game <- ZIO.fromOption(state.games.get(id))
            _ <- game.stop
          } yield ()).ignore.as(state.copy(games = state.games.removed(id)) -> ().asInstanceOf[A])
      }
  }

  private val randomGameId: URIO[Random, String] = {
    val possibleChars = 'a' to 'z'
    val mask = "###-###-###"
    val maskChar = '#'

    val randomPossibleChar = nextIntBounded(possibleChars.length).map(possibleChars(_))

    def aux(mask: String): ZIO[Random, Nothing, String] = {
      if (!mask.contains(maskChar)) UIO(mask)
      else
        for {
          c <- randomPossibleChar
          ret <- aux(mask.replace(maskChar, c))
        } yield ret
    }

    aux(mask)
  }
}
