package zodo.jeopardy.actors

import zio._
import zio.actors._
import zio.logging._
import zio.random._
import zodo.jeopardy.actors.GameActor.InputMessage._
import zodo.jeopardy.actors.GameActor.State._
import zodo.jeopardy.actors.GameActor.State.Stage._
import zodo.jeopardy.model.PackModel

object GameActor {

  type GameActorRef = ActorRef[InputMessage]
  type PlayerActorRef = ActorRef[OutgoingMessage]

  sealed trait InputMessage[+_]
  object InputMessage {
    case class JoinPlayer(id: String, name: String, reply: PlayerActorRef) extends InputMessage[Unit]
    case object StartGame extends InputMessage[Unit]
    case class ChooseQuestion(playerId: String, questionId: String) extends InputMessage[Unit]
  }

  sealed trait OutgoingMessage[+_]
  object OutgoingMessage {
    case class PlayerListUpdated(players: Seq[Player]) extends OutgoingMessage[Unit]
    case class StageUpdated(stage: Stage) extends OutgoingMessage[Unit]
  }

  case class State(
    pack: PackModel.Pack,
    players: Seq[PlayerContainer],
    stage: Stage
  )
  object State {
    sealed trait Stage
    object Stage {
      case object WaitingForStart extends Stage
      case class InRound(
        round: PackModel.Round,
        takenQuestions: Set[String],
        activePlayer: String
      ) extends Stage
      case class InQuestion(
        question: PackModel.Question
      ) extends Stage
    }

    case class PlayerContainer(
      player: Player,
      reply: PlayerActorRef
    )

    case class Player(
      id: String,
      name: String,
      score: Int
    )
  }

  def initState(pack: PackModel.Pack): State = State(pack, Seq(), WaitingForStart)

  type Env = Logging with Random

  val handler = new Actor.Stateful[Env, State, InputMessage] {

    override def receive[A](state: State, msg: InputMessage[A], context: Context): RIO[Env, (State, A)] = {
      def broadcast(m: OutgoingMessage[Unit]) = ZIO.foreach(state.players)(p => p.reply ! m)
      def randomPlayer: URIO[Random, Player] =
        nextIntBounded(state.players.size).map(idx => state.players(idx).player)

      (state, msg) match {
        case (_, JoinPlayer(id, name, reply)) =>
          for {
            _ <- log.debug(s"GameActor <- JoinPlayer($id, $name)")
            newPlayers = state.players :+ PlayerContainer(Player(id, name, 0), reply)
            event = OutgoingMessage.PlayerListUpdated(newPlayers.map(_.player))
            _ <- broadcast(event)
            _ <- reply ! event
            _ <- reply ! OutgoingMessage.StageUpdated(state.stage)
          } yield state.copy(players = newPlayers) -> ()
        case (_, StartGame) =>
          for {
            _            <- log.debug(s"GameActor <- StartGame")
            activePlayer <- randomPlayer
            newStage = InRound(state.pack.rounds.head, Set(), activePlayer.id)
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage))
          } yield state.copy(stage = newStage) -> ()

        case (State(_, _, InRound(round, taken, activePlayer)), ChooseQuestion(playerId, questionId)) =>
          (for {
            _        <- log.debug(s"GameActor <- ChooseQuestion($questionId)")
            question <- ZIO.fromOption(round.themes.flatMap(_.questions).find(_.id == questionId))
            _        <- log.debug(s"Question found! $question")
            _        <- ZIO.fail(()).when(taken.contains(questionId))
            _        <- log.debug(s"Question is new")
            _        <- log.debug(s"active $activePlayer playerId $playerId")
            _        <- ZIO.fail(()).when(activePlayer != playerId)
            _        <- log.debug(s"Player can choose")
            newStage = InQuestion(question)
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage))
            _ <- log.debug(s"Broadcasted new stage")
          } yield state.copy(stage = newStage) -> ()).catchAll(_ => UIO(state -> ()))

        case (_, _: ChooseQuestion) => UIO(state -> ())
      }
    }
  }

}
