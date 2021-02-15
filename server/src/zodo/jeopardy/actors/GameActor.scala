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
import zodo.jeopardy.model.GameEvent.{CountdownModel, CountdownUpdated}
import zodo.jeopardy.model._

import java.util.UUID

object GameActor {

  type Env = Logging with Random with Clock with Has[GameConfig]
  type CountdownFiber = Fiber.Runtime[Throwable, Unit]
  type CountdownId = String

  case class State(
    pack: PackModel.Pack,
    players: Seq[Player],
    stage: Stage,
    countdowns: Map[CountdownId, Countdown]
  ) {
    def withStage(stage: Stage): State = copy(stage = stage)
    def withRoundStage(roundStage: RoundStage): State = stage match {
      case r: Round => copy(stage = r.copy(stage = roundStage))
      case _        => this
    }
    def withCd(cdId: CountdownId, cd: Countdown): State = copy(countdowns = countdowns.updated(cdId, cd))
    def withoutCd(cdId: CountdownId): State = copy(countdowns = countdowns.removed(cdId))
    def withPlayerScore(playerId: PlayerId, adjustScore: Int => Int): State = {
      val newPlayers = players
        .map(p => if (p.id == playerId) p.copy(score = adjustScore(p.score)) else p)
      copy(players = newPlayers)
    }
  }

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
        case class Idle(cdId: CountdownId) extends RoundStage
        case class Question(question: PackModel.Question, cdId: CountdownId) extends RoundStage
        case class AwaitingAnswer(questionStage: Question, answeringPlayer: String, cdId: CountdownId)
            extends RoundStage
        case class Answer(answer: PackModel.Answers) extends RoundStage
      }
    }

    case class Player(
      id: PlayerId,
      name: String,
      score: Int,
      reply: PlayerActorRef,
      disconnected: Boolean = false
    ) {
      def toMessage: GameEvent.Player = GameEvent.Player(id, name, score, disconnected)
    }

    case class Countdown(
      max: Int,
      value: Int,
      fiber: CountdownFiber,
      postAction: RIO[Env, Unit]
    ) {
      def isFinished(tick: Int): Boolean = tick == max
    }
  }

  def initState(pack: PackModel.Pack): State = State(pack, Seq(), BeforeStart, Map())

  val handler: Actor.Stateful[Env, State, GameCommand] = new Actor.Stateful[Env, State, GameCommand] {
    override def receive[A](state: State, msg: GameCommand[A], context: Context): RIO[Env, (State, A)] = {
      for {
        _      <- log.debug(s"GameActor <- $msg")
        config <- ZIO.service[GameConfig]
        onReceive = new OnReceive(state, msg, context, config)
        newState <- onReceive.handle
      } yield newState -> ().asInstanceOf[A]
    }
  }

  private class OnReceive(state: State, msg: GameCommand[_], context: Context, config: GameConfig) {

    def handle: RIO[Env, State] =
      (state.stage, msg) match {
        case (_, m: AddPlayer)                                   => handleAddPlayer(m)
        case (_, DisconnectPlayer(playerId))                     => handlePlayerDisconnect(playerId)
        case (_, Start)                                          => handleStart
        case (r @ Round(Idle(cd), _, _, _), m: SelectQuestion)   => handleSelectQuestion(r, m, cd)
        case (r @ Round(q: Question, _, _, _), HitButton(pId))   => handleHitButton(r, q, pId)
        case (Round(_: AwaitingAnswer, _, _, _), HitButton(pId)) => handleMissHitButton(pId)
        case (r @ Round(AwaitingAnswer(q, apId, cd), _, _, _), GiveAnswer(pId, a)) if pId == apId =>
          handleGiveAnswer(r, q, pId, a, cd)
        case (r: Round, m: ShowAnswer)        => handleShowAnswer(r, m)
        case (r: Round, ReturnToRound)        => handleReturnToRound(r)
        case (_, TickCountdown(tick, id))     => handleTickCountdown(tick, id)
        case (r: Round, ChooseRandomQuestion) => handleChooseRandomQuestion(r)
        case (Round(AwaitingAnswer(q, apId, cd), _, _, _), PlayerDontKnowAnswer) =>
          handlePlayerDontKnowAnswer(q.question, apId, cd)

        case s => log.error(s"Unexpected message $msg for state $state").as(state)
      }

    private def handleAddPlayer(m: AddPlayer) = {
      val idx = state.players.indexWhere(_.id == m.id)
      if (idx >= 0) {
        val player = state.players(idx).copy(reply = m.reply, disconnected = false)
        val newState = state.copy(players = state.players.updated(idx, player))
        for {
          _ <- broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          _ <- player.reply ! GameEvent.StageUpdated(toSnapshot(state.stage))
        } yield newState
      } else {
        val player = Player(m.id, m.name, 0, m.reply)
        val newState = state.copy(players = state.players :+ player)
        for {
          _ <- broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          _ <- player.reply ! GameEvent.StageUpdated(toSnapshot(state.stage))
        } yield newState
      }
    }

    private def handlePlayerDisconnect(playerId: String) = {
      val idx = state.players.indexWhere(_.id == playerId)
      if (idx < 0) {
        UIO(state)
      } else {
        val player = state.players(idx)
        val newState = state.copy(players = state.players.updated(idx, player.copy(disconnected = true)))
        for {
          _ <- broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
        } yield newState
      }
    }

    private def handleStart = {
      val alivePlayers = state.players.filterNot(_.disconnected)
      for {
        randomActivePlayer <- nextIntBounded(alivePlayers.size).map(idx => alivePlayers(idx))
        (cdId, cd)         <- setCountdown(config.questionSelectionTimeout)(_ ! GameCommand.ChooseRandomQuestion)
        newStage = Round(Idle(cdId), state.pack.rounds.head, Set(), randomActivePlayer.id)
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state
        .withCd(cdId, cd)
        .withStage(newStage)
    }

    private def handleSelectQuestion(r: Round, m: SelectQuestion, idleCdId: CountdownId) = {
      (
        for {
          question     <- ZIO.fromOption(r.round.themes.flatMap(_.questions).find(_.id == m.questionId))
          _            <- ZIO.fail(()).when(r.takenQuestions.contains(m.questionId))
          _            <- ZIO.fail(()).when(r.activePlayer != m.playerId)
          (qCdId, qCd) <- setCountdown(config.hitTheButtonTimeout)(_ ! ShowAnswer(question))
          newStage = r.copy(stage = Question(question, qCdId))
          _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
          _ <- stoppedCountdown(idleCdId)
        } yield state
          .withCd(qCdId, qCd)
          .withoutCd(idleCdId)
          .withStage(newStage)
      ).orElseSucceed(state)
    }

    private def handleHitButton(r: Round, questionStage: Question, playerId: String) = {
      for {
        _                <- stoppedCountdown(questionStage.cdId)
        _                <- broadcast(GameEvent.PlayerHitTheButton(playerId))
        (answerCdId, cd) <- setCountdown(config.answerTimeout)(_ ! GameCommand.PlayerDontKnowAnswer)
        newStage = r.copy(stage = AwaitingAnswer(questionStage, playerId, answerCdId))
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state
        .withCd(answerCdId, cd)
        .withoutCd(questionStage.cdId)
        .withStage(newStage)
    }

    private def handleMissHitButton(playerId: String) = {
      for {
        _ <- broadcast(GameEvent.PlayerHitTheButton(playerId))
      } yield state
    }

    private def handleGiveAnswer(
      r: Round,
      questionStage: Question,
      playerId: String,
      answer: String,
      answerCdId: CountdownId
    ) = {
      val question = questionStage.question
      if (isCorrect(question.answers, answer)) {
        val newState = state
          .withPlayerScore(playerId, _ + question.price)
          .withStage(
            r.copy(
              takenQuestions = r.takenQuestions + question.id,
              activePlayer = playerId
            )
          )
          .withoutCd(answerCdId)
        for {
          _    <- stoppedCountdown(answerCdId)
          _    <- broadcast(GameEvent.PlayerGaveAnswer(playerId, answer, isCorrect = true))
          _    <- broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          self <- context.self[GameCommand]
          _    <- self ! ShowAnswer(question)
        } yield newState
      } else {
        handleInvalidAnswer(playerId, answer, answerCdId, question)
      }
    }

    def handlePlayerDontKnowAnswer(question: PackModel.Question, playerId: String, answerCdId: CountdownId) = {
      val shrugEmoji = "🤷"
      handleInvalidAnswer(playerId, shrugEmoji, answerCdId, question)
    }

    private def handleInvalidAnswer(
      playerId: String,
      answer: String,
      answerCdId: CountdownId,
      question: PackModel.Question
    ) = {
      for {
        _          <- stoppedCountdown(answerCdId)
        (cdId, cd) <- setCountdown(config.answerTimeout)(_ ! ShowAnswer(question))
        newState = state
          .withPlayerScore(playerId, _ - question.price)
          .withCd(cdId, cd)
          .withoutCd(answerCdId)
          .withRoundStage(Question(question, cdId))
        _ <- broadcast(GameEvent.PlayerGaveAnswer(playerId, answer, isCorrect = false))
        _ <- broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newState.stage)))
      } yield newState
    }

    private def handleShowAnswer(round: Round, msg: ShowAnswer) = {
      val cdId = round.stage match {
        case Question(_, cdId)          => Some(cdId)
        case AwaitingAnswer(_, _, cdId) => Some(cdId)
        case _                          => None
      }

      val newStage =
        round.copy(takenQuestions = round.takenQuestions + msg.question.id, stage = Answer(msg.question.answers))
      for {
        _    <- ZIO.fromOption(cdId).flatMap(stoppedCountdown).ignore
        _    <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
        self <- context.self[GameCommand]
        _    <- (self ! ReturnToRound).delay(3.second).fork
      } yield state.copy(countdowns = state.countdowns.removed(cdId.getOrElse("")), stage = newStage)
    }

    private def handleReturnToRound(r: Round) = {
      val haveMoreQuestions = r.round.themes.flatMap(_.questions).size > r.takenQuestions.size
      val haveMoreRounds = state.pack.rounds.last.id != r.round.id

      for {
        (cdId, cd) <- setCountdown(config.questionSelectionTimeout)(_ ! GameCommand.ChooseRandomQuestion)
        newStage =
          if (haveMoreQuestions) {
            r.copy(stage = Idle(cdId))
          } else if (haveMoreRounds) {
            val newRound = state.pack.rounds(state.pack.rounds.indexWhere(_.id == r.round.id) + 1)
            Round(Idle(cdId), newRound, Set(), r.activePlayer)
          } else {
            Round(Idle(cdId), state.pack.rounds.head, Set(), r.activePlayer)
          }

        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state
        .withStage(newStage)
        .withCd(cdId, cd)
    }

    private def handleTickCountdown(tick: Int, id: CountdownId) = {
      state.countdowns.get(id) match {
        case Some(cd) =>
          if (cd.isFinished(tick)) {
            for {
              _ <- log.debug(s"counted to ${cd.max}! Id: $id")
              _ <- cd.postAction
              _ <- log.debug(s"executed post action! Id: $id")
              _ <- broadcast(GameEvent.CountdownUpdated(None))
            } yield state
              .withoutCd(id)
          } else {
            for {
              _ <- log.debug(s"counting - $tick/${cd.max}! Id: $id")
              _ <- broadcast(CountdownUpdated(Some(CountdownModel(tick, cd.max))))
            } yield state.withCd(id, cd.copy(value = tick))
          }

        case None => log.error(s"can't find counter $id on tick $tick").as(state)
      }
    }

    def handleChooseRandomQuestion(r: Round) = {
      val availableQuestions = r.round.themes.flatMap(_.questions).filterNot(q => r.takenQuestions.contains(q.id))
      for {
        randomQuestion <- nextIntBounded(availableQuestions.length).map(availableQuestions)
        (qCdId, qCd)   <- setCountdown(config.hitTheButtonTimeout)(_ ! ShowAnswer(randomQuestion))
        newStage = r.copy(stage = Question(randomQuestion, qCdId))
        _ <- broadcast(GameEvent.StageUpdated(toSnapshot(newStage)))
      } yield state
        .withCd(qCdId, qCd)
        .withStage(newStage)
    }

    private def broadcast(m: GameEvent[Unit], useState: State = state) =
      ZIO.foreachPar(useState.players.filterNot(_.disconnected))(p => log.debug(s"Sending $m to $p") *> (p.reply ! m))

    private def setCountdown(seconds: Int)(
      afterCountdown: ActorRef[GameCommand] => RIO[Env, Unit]
    ): RIO[Env, (CountdownId, Countdown)] = {
      val id = UUID.randomUUID().toString

      def inBackground(self: ActorRef[GameCommand]) = ZIO
        .foreach_(1 to seconds) { tick =>
          for {
            _ <- ZIO.sleep(1.second)
            _ <- self ! GameCommand.TickCountdown(tick, id)
          } yield ()
        }

      for {
        _     <- log.debug(s"set countdown for $seconds with id $id")
        _     <- broadcast(CountdownUpdated(Some(CountdownModel(0, seconds))))
        self  <- context.self[GameCommand]
        fiber <- inBackground(self).fork
      } yield id -> Countdown(seconds, 0, fiber, afterCountdown(self))
    }

    private def stoppedCountdown(id: CountdownId): URIO[Logging, Unit] = {
      (for {
        _  <- log.debug(s"stop countdown $id")
        cd <- ZIO.fromOption(state.countdowns.get(id))
        _  <- cd.fiber.interrupt
        _  <- broadcast(GameEvent.CountdownUpdated(None))
        _  <- log.debug(s"countdown $id stopped")
      } yield ()).ignore
    }
  }

  private def isCorrect(correctAnswer: PackModel.Answers, actualAnswer: String) = {
    val sanitize = (_: String).replaceAll("[^\\p{L}\\p{N}]+", "").toLowerCase

    correctAnswer.correct.map(sanitize).contains(sanitize(actualAnswer))
  }

  private def toSnapshot(s: Stage): StageSnapshot = s match {
    case BeforeStart => StageSnapshot.BeforeStart
    case r: Round =>
      r.stage match {
        case _: Idle                   => StageSnapshot.Round(r.round, r.takenQuestions, r.activePlayer)
        case Question(question, _)     => StageSnapshot.Question(question)
        case AwaitingAnswer(q, pId, _) => StageSnapshot.AnswerAttempt(q.question, pId)
        case Answer(answer)            => StageSnapshot.Answer(answer)
      }
  }

}
