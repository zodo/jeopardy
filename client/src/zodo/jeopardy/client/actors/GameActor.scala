package zodo.jeopardy.client.actors

import zio.actors.Actor.Stateful
import zio.actors.{ActorRef, Context}
import zio.logging._
import zio.{RIO, ZIO}
import zodo.jeopardy.client.actors.GameActor.InputMessage.{JoinPlayer, StartGame}
import zodo.jeopardy.client.actors.GameActor.State.{GameState, InRound, Player, WaitingForStart}
import zodo.jeopardy.model.PackModel

object GameActor {

  type GameActorRef = ActorRef[InputMessage]
  type PlayerActorRef = ActorRef[OutgoingMessage]

  sealed trait InputMessage[+_]

  object InputMessage {
    case class JoinPlayer(id: String, name: String, reply: PlayerActorRef) extends InputMessage[Unit]
    case object StartGame extends InputMessage[Unit]
  }

  sealed trait OutgoingMessage[+_]

  object OutgoingMessage {
    case class NewPlayerConnected(id: String, name: String) extends OutgoingMessage[Unit]
    case class RoundStarted(round: PackModel.Round) extends OutgoingMessage[Unit]
  }

  case class State(
    pack: PackModel.Pack,
    players: Seq[Player],
    gameState: GameState
  )

  object State {
    case class Player(id: String, name: String, reply: PlayerActorRef)

    sealed trait GameState

    case object WaitingForStart extends GameState

    case class InRound(
      round: PackModel.Round,
      next: Option[PackModel.Round]
    ) extends GameState

  }

  def initState(pack: PackModel.Pack): State = State(pack, Nil, WaitingForStart)

  type Env = Logging

  val handler = new Stateful[Env, State, InputMessage] {

    override def receive[A](state: State, msg: InputMessage[A], context: Context): RIO[Env, (State, A)] = {
      def broadcast(m: OutgoingMessage[Unit]) = ZIO.foreach(state.players)(p => p.reply ! m)

      msg match {
        case JoinPlayer(id, name, reply) =>
          for {
            _ <- log.debug(s"GameActor <- JoinPlayer($id, $name)")
            _ <- broadcast(OutgoingMessage.NewPlayerConnected(id, name))
            _ <- reply ! OutgoingMessage.NewPlayerConnected(id, name)
          } yield state.copy(players = state.players :+ Player(id, name, reply)) -> ()
        case StartGame =>
          for {
            _ <- log.debug(s"GameActor <- StartGame")
            _ <- broadcast(OutgoingMessage.RoundStarted(state.pack.rounds.head))
          } yield state.copy(gameState = InRound(state.pack.rounds.head, state.pack.rounds.drop(1).headOption)) -> ()
      }
    }
  }

}
