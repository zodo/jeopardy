package zodo.jeopardy.actors.game
import zio.RIO
import zodo.jeopardy.actors.PlayerActorRef
import zodo.jeopardy.actors.game.State.Stage.RoundStage._
import zodo.jeopardy.actors.game.State.Stage.{Round, RoundStage}
import zodo.jeopardy.actors.game.State.{Countdown, Player, Stage}
import zodo.jeopardy.model.{GameEvent, PackModel, PlayerId, StageSnapshot}

case class State(
  id: String,
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
  def withPlayers(map: Player => Player): State = {
    copy(players = players.map(map))
  }
}

object State {
  sealed trait Stage {
    def toSnapshot: StageSnapshot
  }
  object Stage {
    case object BeforeStart extends Stage {
      override def toSnapshot = StageSnapshot.BeforeStart
    }
    case class Round(
      stage: RoundStage,
      model: PackModel.Round,
      takenQuestions: Set[String],
      activePlayer: String,
      previousAnswers: AnswersSummary
    ) extends Stage {
      override def toSnapshot = stage match {
        case _: Idle =>
          StageSnapshot.Round(
            model,
            takenQuestions,
            activePlayer,
            previousAnswers.players,
            previousAnswers.triedToAppeal
          )
        case Question(question, _)        => StageSnapshot.Question(question)
        case ReadyForHit(question, _)     => StageSnapshot.ReadyForHit(question)
        case AwaitingAnswer(q, pId, _, _) => StageSnapshot.AnswerAttempt(q, pId)
        case Answer(answer)               => StageSnapshot.Answer(answer)
        case Appeal(_, m, a, p, r)        => StageSnapshot.Appeal(m, a, p, r.filter(_._2).keySet, r.filterNot(_._2).keySet)
        case AppealResult(r)              => StageSnapshot.AppealResult(r)
      }
    }

    sealed trait RoundStage
    object RoundStage {
      case class Idle(cdId: CountdownId) extends RoundStage
      case class Question(model: PackModel.Question, readyPlayers: Map[PlayerId, Int]) extends RoundStage
      case class ReadyForHit(model: PackModel.Question, cdId: CountdownId) extends RoundStage
      case class AwaitingAnswer(
        model: PackModel.Question,
        answeringPlayer: String,
        cdId: CountdownId,
        questionSecondsPassed: Int
      ) extends RoundStage
      case class Answer(model: PackModel.Answers) extends RoundStage
      case class Appeal(
        cdId: CountdownId,
        model: PackModel.Question,
        answer: String,
        playerId: PlayerId,
        resolutions: Map[PlayerId, Boolean]
      ) extends RoundStage
      case class AppealResult(resolution: Boolean) extends RoundStage
      def unapply(state: State): Option[(RoundStage, Round)] = {
        state.stage match {
          case r: Round => Some((r.stage, r))
          case _        => None
        }
      }
    }

    case class AnswersSummary(
      model: Option[PackModel.Question],
      answers: Seq[PlayerAnswer],
      triedToAppeal: Set[PlayerId] = Set()
    ) {
      def withAnswer(a: PlayerAnswer) = copy(answers = answers :+ a)
      val players: Map[PlayerId, Boolean] = answers
        .map(a => a.playerId -> a.isCorrect)
        .toMap

      def rollbackScores(appealPlayerId: PlayerId): Map[PlayerId, Int] = {
        val idx = answers.indexWhere(_.playerId == appealPlayerId)
        if (idx >= 0) {
          answers
            .drop(idx)
            .map(a => a.playerId -> a.scoreDiff)
            .toMap
        } else Map()
      }
    }

    case class PlayerAnswer(
      playerId: PlayerId,
      text: String,
      isCorrect: Boolean,
      scoreDiff: Int
    )
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

  def unapply(state: State): Option[Stage] = Some(state.stage)
}
