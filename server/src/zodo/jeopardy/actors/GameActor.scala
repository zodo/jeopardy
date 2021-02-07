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
        stage: RoundStage,
        round: PackModel.Round,
        takenQuestions: Set[String],
        activePlayer: String
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

  val handler: Actor.Stateful[Env, State, GameCommand] = new Actor.Stateful[Env, State, GameCommand] {
    override def receive[A](state: State, msg: GameCommand[A], context: Context): RIO[Env, (State, A)] = {
      val onReceive = new OnReceive(state, msg, context)

      for {
        _        <- log.debug(s"GameActor <- $msg")
        newState <- onReceive.handle
      } yield newState -> ().asInstanceOf[A]
    }
  }

  private class OnReceive(state: State, msg: GameCommand[_], context: Context) {

    def handle: RIO[Env, State] =
      (state.stage, msg) match {
        case (_, m: AddPlayer)                                   => handleAddPlayer(m)
        case (_, Start)                                          => handleStart
        case (r @ Round(Idle, _, _, _), m: SelectQuestion)       => handleSelectQuestion(r, m)
        case (r @ Round(Question(q), _, _, _), HitButton(pId))   => handleHitButton(r, q, pId)
        case (Round(_: AwaitingAnswer, _, _, _), HitButton(pId)) => handleMissHitButton(pId)
        case (r @ Round(AwaitingAnswer(q, apId), _, _, _), GiveAnswer(pId, a)) if pId == apId =>
          handleGiveAnswer(r, q, pId, a)
        case (r: Round, m: ShowAnswer) => handleShowAnswer(r, m)
        case (r: Round, ReturnToRound) => handleReturnToRound(r)

        case s => log.error(s"Unexpected message $msg for state $state").as(state)
      }

    private def handleAddPlayer(m: AddPlayer) = {
      val player = Player(m.id, m.name, 0, m.reply)
      for {
        _ <- broadcast(GameEvent.PlayerAdded(player.toMessage))
        _ <- ZIO.foreach_(state.players :+ player) { p =>
          player.reply ! GameEvent.PlayerAdded(p.toMessage)
        }
        _ <- player.reply ! GameEvent.StageUpdated(toSnapshot(state.stage))
      } yield state.copy(players = state.players :+ player)
    }

    private def handleStart = {
      for {
        randomActivePlayer <- nextIntBounded(state.players.size).map(idx => state.players(idx))
        newStage = Round(Idle, state.pack.rounds.head, Set(), randomActivePlayer.id)
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state.copy(stage = newStage)
    }

    private def handleSelectQuestion(r: Round, m: SelectQuestion) = {
      (
        for {
          question <- ZIO.fromOption(r.round.themes.flatMap(_.questions).find(_.id == m.questionId))
          _        <- ZIO.fail(()).when(r.takenQuestions.contains(m.questionId))
          _        <- ZIO.fail(()).when(r.activePlayer != m.playerId)
          newStage = r.copy(stage = Question(question))
          _         <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
          self      <- context.self[GameCommand]
          countdown <- setCountdown(10)(self ! ShowAnswer(question, onTimeout = true))
        } yield state.copy(stage = newStage, countdown = Some(countdown))
      ).orElseSucceed(state)
    }

    private def handleHitButton(r: Round, question: PackModel.Question, playerId: String) = {
      for {
        _ <- stopCountdown
        _ <- broadcast(GameEvent.PlayerHitTheButton(playerId))
        newStage = r.copy(stage = AwaitingAnswer(question, playerId))
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state.copy(stage = newStage, countdown = None)
    }

    private def handleMissHitButton(playerId: String) = {
      for {
        _ <- broadcast(GameEvent.PlayerHitTheButton(playerId))
      } yield state
    }

    private def handleGiveAnswer(r: Round, question: PackModel.Question, playerId: String, answer: String) = {
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
        } yield newState
      } else {
        val newState = withUpdatedPlayerScore(state, playerId, _ - question.price)
          .copy(stage = r.copy(stage = Question(question)))
        for {
          _    <- broadcast(GameEvent.PlayerGaveAnswer(playerId, answer, isCorrect = false))
          _    <- broadcast(GameEvent.PlayerScoreUpdated(playerId, -question.price))
          _    <- broadcast(GameEvent.StageUpdated(toSnapshot(newState.stage)))
          self <- context.self[GameCommand]
          cd   <- setCountdown(10)(self ! ShowAnswer(question))
        } yield newState.copy(countdown = Some(cd))
      }
    }

    private def handleShowAnswer(round: Round, msg: ShowAnswer) = {
      (round.stage, msg.onTimeout) match {
        case (_: AwaitingAnswer, false) | (_: Question, true) =>
          val newStage =
            round.copy(takenQuestions = round.takenQuestions + msg.question.id, stage = Answer(msg.question.answers))
          for {
            _    <- stopCountdown
            _    <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
            self <- context.self[GameCommand]
            _    <- (self ! ReturnToRound).delay(3.second).fork
          } yield state.copy(stage = newStage, countdown = None)
        case _ => UIO(state)
      }
    }

    private def handleReturnToRound(r: Round) = {
      val haveMoreQuestions = r.round.themes.flatMap(_.questions).size > r.takenQuestions.size
      val newStage = if (haveMoreQuestions) {
        r.copy(stage = Idle)
      } else {
        val haveMoreRounds = state.pack.rounds.last.id != r.round.id
        if (haveMoreRounds) {
          val newRound = state.pack.rounds(state.pack.rounds.indexWhere(_.id == r.round.id) + 1)
          Round(Idle, newRound, Set(), r.activePlayer)
        } else {
          Round(Idle, state.pack.rounds.head, Set(), r.activePlayer)
        }
      }

      for {
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state.copy(stage = newStage)
    }

    private def broadcast(m: GameEvent[Unit]) = ZIO.foreachPar(state.players)(p => p.reply ! m)

    private def setCountdown[R](seconds: Int)(afterCountdown: RIO[R, Unit]): URIO[R with Clock, CountdownFiber] = {
      ZIO
        .foreach_(0 until seconds) { tick =>
          for {
            _ <- broadcast(CountdownUpdated(Some(Countdown(tick, seconds))))
            _ <- ZIO.sleep(1.second)
          } yield ()
        } *>
        broadcast(CountdownUpdated(None)) *>
        afterCountdown
    }.fork
    private val stopCountdown = ZIO.fromOption(state.countdown).flatMap(_.interrupt).ignore
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
