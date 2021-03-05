package zodo.jeopardy.actors.game.handlers

import zio._
import zio.duration.durationInt
import zodo.jeopardy.actors.game.State
import zodo.jeopardy.actors.game.State.Stage.Round
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Answer, AwaitingAnswer, Question, ReadyForHit}
import zodo.jeopardy.model.GameCommand.{ReturnToRound, ShowAnswer}
import zodo.jeopardy.model.GameEvent

object ShowAnswerHandler extends Handler[ShowAnswer] {
  override def process(m: ShowAnswer, ctx: HandlerContext) = {
    case state @ State(round: Round) =>
      val cdId = round.stage match {
        case ReadyForHit(_, cdId)          => Some(cdId)
        case AwaitingAnswer(_, _, cdId, _) => Some(cdId)
        case _                             => None
      }

      val newStage =
        round.copy(takenQuestions = round.takenQuestions + m.question.id, stage = Answer(m.question.answers))
      for {
        _ <- ZIO.fromOption(cdId).flatMap(ctx.stoppedCountdown).ignore
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
        _ <- (ctx.self ! ReturnToRound).delay(3.second).fork
      } yield state.copy(countdowns = state.countdowns.removed(cdId.getOrElse("")), stage = newStage)
  }
}
