package zodo.jeopardy.actors

import zio._
import zio.actors._
import zio.clock.Clock
import zio.duration.durationInt
import zio.logging._
import zio.random._
import zodo.jeopardy.actors.GameActor.State.Stage.RoundStage._
import zodo.jeopardy.actors.GameActor.State.Stage._
import zodo.jeopardy.actors.GameActor.State._
import zodo.jeopardy.model.GameCommand.{ShowAnswer, _}
import zodo.jeopardy.model.GameEvent.{Countdown, CountdownUpdated}
import zodo.jeopardy.model.{GameCommand, GameEvent, PackModel, StageSnapshot}

object GameActor {

  type Env = Logging with Random with Clock
  type CountdownFiber = Fiber.Runtime[Throwable, Unit]

  case class State(
    pack: PackModel.Pack,
    players: Seq[Player],
    stage: Stage,
    countdown: Option[CountdownFiber]
  )

  object State {
    sealed trait Stage
    object Stage {
      case object BeforeStart extends Stage
      case class Round(
        round: PackModel.Round,
        takenQuestions: Set[String],
        activePlayer: String,
        stage: RoundStage
      ) extends Stage

      sealed trait RoundStage
      object RoundStage {
        case object Idle extends RoundStage
        case class Question(question: PackModel.Question) extends RoundStage
        case class AwaitingAnswer(question: PackModel.Question, answeringPlayer: String) extends RoundStage
        case class Answer(answer: PackModel.Answers) extends RoundStage
      }
    }

    case class Player(
      id: String,
      name: String,
      score: Int,
      reply: PlayerActorRef
    ) {
      def toMessage: GameEvent.Player = GameEvent.Player(id, name)
    }
  }

  def initState(pack: PackModel.Pack): State = State(pack, Seq(), BeforeStart, None)

  val handler = new Actor.Stateful[Env, State, GameCommand] {

    override def receive[A](state: State, msg: GameCommand[A], context: Context): RIO[Env, (State, A)] = {
      def broadcast(m: GameEvent[Unit]) = ZIO.foreachPar(state.players)(p => p.reply ! m)
      def setCountdown[R](seconds: Int)(atTheEnd: RIO[R, Unit]): URIO[R with Clock, CountdownFiber] = {
        ZIO
          .foreach_(0 until seconds) { tick =>
            for {
              _ <- broadcast(CountdownUpdated(Some(Countdown(tick, seconds))))
              _ <- ZIO.sleep(1.second)
            } yield ()
          } *>
          atTheEnd
      }.fork
      val stopCountdown = ZIO.fromOption(state.countdown).flatMap(_.interrupt).ignore

      (state.stage, msg) match {
        case (_, AddPlayer(id, name, reply)) =>
          for {
            _ <- log.debug(s"GameActor <- JoinPlayer($id, $name)")
            player = Player(id, name, 0, reply)
            _ <- broadcast(GameEvent.PlayerAdded(player.toMessage))
            _ <- ZIO.foreach_(state.players :+ player) { p =>
              reply ! GameEvent.PlayerAdded(p.toMessage)
            }
            _ <- reply ! GameEvent.StageUpdated(toSnapshot(state.stage))
          } yield state.copy(players = state.players :+ player) -> ()
        case (_, Start) =>
          for {
            _            <- log.debug(s"GameActor <- StartGame")
            activePlayer <- nextIntBounded(state.players.size).map(idx => state.players(idx))
            newStage = Round(state.pack.rounds.head, Set(), activePlayer.id, Idle)
            _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
          } yield state.copy(stage = newStage) -> ()

        case (r @ Round(round, taken, activePlayer, Idle), SelectQuestion(playerId, questionId)) =>
          (for {
            _        <- log.debug(s"GameActor <- ChooseQuestion($questionId)")
            question <- ZIO.fromOption(round.themes.flatMap(_.questions).find(_.id == questionId))
            _        <- ZIO.fail(()).when(taken.contains(questionId))
            _        <- ZIO.fail(()).when(activePlayer != playerId)
            newStage = r.copy(stage = Question(question))
            _         <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
            self      <- context.self[GameCommand]
            countdown <- setCountdown(10)(self ! ShowAnswer(question, onTimeout = true))
          } yield state.copy(stage = newStage, countdown = Some(countdown)) -> ()).catchAll(_ => UIO(state -> ()))

        case (r @ Round(_, _, _, Question(question)), HitButton(playerId)) =>
          for {
            _ <- stopCountdown
            _ <- broadcast(GameEvent.PlayerHitTheButton(playerId))
            newStage = r.copy(stage = AwaitingAnswer(question, playerId))
            _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
          } yield state.copy(stage = newStage, countdown = None) -> ()

        case (Round(_, _, _, _: AwaitingAnswer), HitButton(playerId)) =>
          for {
            _ <- broadcast(GameEvent.PlayerHitTheButton(playerId))
          } yield state -> ()

        case (r @ Round(_, _, _, AwaitingAnswer(question, answeringPlayer)), GiveAnswer(playerId, answer))
            if playerId == answeringPlayer =>
          if (isCorrect(question.answers, answer)) {
            val newState = withUpdatedPlayerScore(state, playerId, _ + question.price)
              .copy(stage =
                r.copy(
                  takenQuestions = r.takenQuestions + question.id,
                  activePlayer = playerId
                )
              )
            for {
              _    <- broadcast(GameEvent.PlayerGaveAnswer(playerId, answer, isCorrect = true))
              _    <- broadcast(GameEvent.PlayerScoreUpdated(playerId, question.price))
              self <- context.self[GameCommand]
              _    <- self ! ShowAnswer(question)
            } yield newState -> ()
          } else {
            val newState = withUpdatedPlayerScore(state, playerId, _ - question.price)
              .copy(stage = r.copy(stage = Question(question)))
            for {
              _ <- broadcast(GameEvent.PlayerGaveAnswer(playerId, answer, isCorrect = false))
              _ <- broadcast(GameEvent.PlayerScoreUpdated(playerId, -question.price))
              _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newState.stage)))
            } yield newState -> ()
          }

        case (r @ Round(_, _, _, stage), ShowAnswer(question, onTimeout)) =>
          def updateState = {
            val newStage = r.copy(takenQuestions = r.takenQuestions + question.id, stage = Answer(question.answers))
            for {
              _    <- stopCountdown
              _    <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
              self <- context.self[GameCommand]
              _    <- (self ! ReturnToRound).delay(5.second).fork
            } yield state.copy(stage = newStage, countdown = None) -> ()
          }

          (stage, onTimeout) match {
            case (_: AwaitingAnswer, false) => updateState
            case (_: Question, true)        => updateState
            case _                          => UIO(state -> ())
          }

        case (r @ Round(round, tq, ap, _), ReturnToRound) =>
          val haveMoreQuestions = round.themes.flatMap(_.questions).size > tq.size
          val newStage = if (haveMoreQuestions) {
            r.copy(stage = Idle)
          } else {
            val haveMoreRounds = state.pack.rounds.last.id == round.id
            if (haveMoreRounds) {
              val newRound = state.pack.rounds(state.pack.rounds.indexWhere(_.id == round.id) + 1)
              Round(newRound, Set(), ap, Idle)
            } else {
              ???
            }
          }

          for {
            _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
          } yield state.copy(stage = newStage) -> ()

        case s => log.error(s"unexpected GameActor <- $s").as(state -> ().asInstanceOf[A])
      }
    }
  }

  private def isCorrect(correctAnswer: PackModel.Answers, actualAnswer: String) = {
    val sanitize = (_: String).replaceAll("[^\\p{L}\\p{N}]+", "").toLowerCase

    correctAnswer.correct.map(sanitize).contains(sanitize(actualAnswer))
  }

  private def withUpdatedPlayerScore(state: State, playerId: String, adjustScore: Int => Int) = {
    val newPlayers = state.players
      .map(p => if (p.id == playerId) p.copy(score = adjustScore(p.score)) else p)
    state.copy(players = newPlayers)
  }

  private def toSnapshot(s: Stage): StageSnapshot = s match {
    case BeforeStart => StageSnapshot.BeforeStart
    case r: Round =>
      r.stage match {
        case Idle                   => StageSnapshot.Round(r.round, r.takenQuestions, r.activePlayer)
        case Question(question)     => StageSnapshot.Question(question)
        case AwaitingAnswer(q, pId) => StageSnapshot.AnswerAttempt(q, pId)
        case Answer(answer)         => StageSnapshot.Answer(answer)
      }
  }

}
