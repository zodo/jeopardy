package zodo.jeopardy.actors

import zio._
import zio.actors._
import zio.clock.Clock
import zio.duration.durationInt
import zio.logging._
import zio.random._
import zodo.jeopardy.actors.GameActor.InputMessage._
import zodo.jeopardy.actors.GameActor.OutgoingMessage.SimpleStage
import zodo.jeopardy.actors.GameActor.State.Stage.RoundStage._
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
    case class HitButton(playerId: String) extends InputMessage[Unit]
    case class Answer(playerId: String, answer: String) extends InputMessage[Unit]
    case object BackToRound extends InputMessage[Unit]
  }

  sealed trait OutgoingMessage[+_]
  object OutgoingMessage {
    case class NewPlayerConnected(player: Player) extends OutgoingMessage[Unit]
    case class PlayerScoreUpdated(playerId: String, scoreDiff: Int) extends OutgoingMessage[Unit]
    case class PlayerHitTheButton(playerId: String) extends OutgoingMessage[Unit]
    case class StageUpdated(stage: SimpleStage) extends OutgoingMessage[Unit]
    case class PlayerHasAnswer(playerId: String, answer: String, isCorrect: Boolean) extends OutgoingMessage[Unit]

    sealed trait SimpleStage
    object SimpleStage {
      case object WaitingForStart extends SimpleStage
      case class InRound(round: PackModel.Round, takenQuestions: Set[String], activePlayer: String) extends SimpleStage
      case class InQuestion(question: PackModel.Question) extends SimpleStage
      case class InAwaitingAnswer(question: PackModel.Question, activePlayer: String) extends SimpleStage
      case class InShowAnswer(answer: PackModel.Answers) extends SimpleStage
    }
  }

  case class State(
    pack: PackModel.Pack,
    players: Seq[PlayerContainer],
    stage: Stage
  )
  object State {
    sealed trait Stage {
      def toSimple: SimpleStage
    }
    object Stage {
      case object WaitingForStart extends Stage {
        def toSimple = SimpleStage.WaitingForStart
      }
      case class Round(
        round: PackModel.Round,
        takenQuestions: Set[String],
        activePlayer: String,
        stage: RoundStage
      ) extends Stage {
        def toSimple: SimpleStage = stage match {
          case Idle                   => SimpleStage.InRound(round, takenQuestions, activePlayer)
          case Question(question)     => SimpleStage.InQuestion(question)
          case AwaitingAnswer(q, pId) => SimpleStage.InAwaitingAnswer(q, pId)
          case ShowAnswer(answer)     => SimpleStage.InShowAnswer(answer)
        }
      }

      sealed trait RoundStage
      object RoundStage {
        case object Idle extends RoundStage
        case class Question(question: PackModel.Question) extends RoundStage
        case class AwaitingAnswer(question: PackModel.Question, answeringPlayer: String) extends RoundStage
        case class ShowAnswer(answer: PackModel.Answers) extends RoundStage
      }
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

  type Env = Logging with Random with Clock

  val handler = new Actor.Stateful[Env, State, InputMessage] {

    override def receive[A](state: State, msg: InputMessage[A], context: Context): RIO[Env, (State, A)] = {
      def broadcast(m: OutgoingMessage[Unit]) = ZIO.foreachPar(state.players)(p => p.reply ! m)
      def randomPlayer: URIO[Random, Player] =
        nextIntBounded(state.players.size).map(idx => state.players(idx).player)

      (state, msg) match {
        case (_, JoinPlayer(id, name, reply)) =>
          for {
            _ <- log.debug(s"GameActor <- JoinPlayer($id, $name)")
            playerContainer = PlayerContainer(Player(id, name, 0), reply)
            _ <- broadcast(OutgoingMessage.NewPlayerConnected(playerContainer.player))
            _ <- ZIO.foreach_(state.players :+ playerContainer) { p =>
              reply ! OutgoingMessage.NewPlayerConnected(p.player)
            }
            _ <- reply ! OutgoingMessage.StageUpdated(state.stage.toSimple)
          } yield state.copy(players = state.players :+ playerContainer) -> ()
        case (_, StartGame) =>
          for {
            _            <- log.debug(s"GameActor <- StartGame")
            activePlayer <- randomPlayer
            newStage = Round(state.pack.rounds.head, Set(), activePlayer.id, Idle)
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage.toSimple))
          } yield state.copy(stage = newStage) -> ()

        case (
              State(_, _, r @ Round(round, taken, activePlayer, Idle)),
              ChooseQuestion(playerId, questionId)
            ) =>
          (for {
            _        <- log.debug(s"GameActor <- ChooseQuestion($questionId)")
            question <- ZIO.fromOption(round.themes.flatMap(_.questions).find(_.id == questionId))
            _        <- log.debug(s"Question found! $question")
            _        <- ZIO.fail(()).when(taken.contains(questionId))
            _        <- log.debug(s"Question is new")
            _        <- log.debug(s"active $activePlayer playerId $playerId")
            _        <- ZIO.fail(()).when(activePlayer != playerId)
            _        <- log.debug(s"Player can choose")
            newStage = r.copy(stage = Question(question))
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage.toSimple))
            _ <- log.debug(s"Broadcasted new stage")
          } yield state.copy(stage = newStage) -> ()).catchAll(_ => UIO(state -> ()))

        case (State(_, _, r @ Round(_, _, _, Question(question))), HitButton(playerId)) =>
          for {
            _ <- broadcast(OutgoingMessage.PlayerHitTheButton(playerId))
            newStage = r.copy(stage = AwaitingAnswer(question, playerId))
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage.toSimple))
          } yield state.copy(stage = newStage) -> ()

        case (State(_, _, Round(_, _, _, _: AwaitingAnswer)), HitButton(playerId)) =>
          for {
            _ <- broadcast(OutgoingMessage.PlayerHitTheButton(playerId))
          } yield state -> ()

        case (
              State(_, _, r @ Round(_, _, _, AwaitingAnswer(question, answeringPlayer))),
              Answer(playerId, answer)
            ) if playerId == answeringPlayer =>
          if (isCorrect(question.answers, answer)) {
            val newState = withUpdatedPlayerScore(state, playerId, _ + question.price)
              .copy(stage =
                r.copy(
                  takenQuestions = r.takenQuestions + question.id,
                  activePlayer = playerId,
                  stage = ShowAnswer(question.answers)
                )
              )
            for {
              _    <- broadcast(OutgoingMessage.PlayerHasAnswer(playerId, answer, isCorrect = true))
              _    <- broadcast(OutgoingMessage.PlayerScoreUpdated(playerId, question.price))
              _    <- broadcast(OutgoingMessage.StageUpdated(newState.stage.toSimple))
              self <- context.self[InputMessage]
              _    <- (self ! BackToRound).delay(5.second).fork
            } yield newState -> ()
          } else {
            val newState = withUpdatedPlayerScore(state, playerId, _ - question.price)
              .copy(stage = r.copy(stage = Question(question)))
            for {
              _ <- broadcast(OutgoingMessage.PlayerHasAnswer(playerId, answer, isCorrect = false))
              _ <- broadcast(OutgoingMessage.PlayerScoreUpdated(playerId, -question.price))
              _ <- broadcast(OutgoingMessage.StageUpdated(newState.stage.toSimple))
            } yield newState -> ()
          }

        case (State(pack, _, r @ Round(round, tq, ap, _)), BackToRound) =>
          val haveMoreQuestions = round.themes.flatMap(_.questions).size > tq.size
          val newStage = if (haveMoreQuestions) {
            r.copy(stage = Idle)
          } else {
            val haveMoreRounds = pack.rounds.last.id == round.id
            if (haveMoreRounds) {
              val newRound = pack.rounds(pack.rounds.indexWhere(_.id == round.id) + 1)
              Round(newRound, Set(), ap, Idle)
            } else {
              ???
            }
          }

          for {
            _ <- broadcast(OutgoingMessage.StageUpdated(newStage.toSimple))
          } yield state.copy(stage = newStage) -> ()

        case s => log.error(s"unexpected GameActor <- $s").as(state -> ().asInstanceOf[A])
      }
    }
  }

  private def isCorrect(correctAnswer: PackModel.Answers, actualAnswer: String) = {
    val sanitize = (_: String).replaceAll("[^\\p{L}\\p{N}]+", "").toLowerCase

    correctAnswer.correct.map(sanitize).contains(sanitize(actualAnswer))
  }

  private def withUpdatedPlayerScore(state: State, playerId: String, score: Int => Int) = {
    val newPlayers = state.players
      .map(p => if (p.player.id == playerId) p.copy(player = p.player.copy(score = score(p.player.score))) else p)
    state.copy(players = newPlayers)
  }

}
