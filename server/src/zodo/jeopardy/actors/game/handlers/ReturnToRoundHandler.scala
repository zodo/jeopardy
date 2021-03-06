package zodo.jeopardy.actors.game.handlers

import zio.URIO
import zio.random.Random
import zodo.jeopardy.actors.game.State
import zodo.jeopardy.actors.game.State.Stage.Round
import zodo.jeopardy.actors.game.State.Stage.RoundStage.Idle
import zodo.jeopardy.model.{GameCommand, GameEvent}

object ReturnToRoundHandler extends Handler[Unit] {
  override def process(m: Unit, ctx: HandlerContext) = {
    case state @ State(round: Round) =>
      val hasMoreQuestions = round.model.themes.flatMap(_.questions).size > round.takenQuestions.size
      val hasMoreRounds = state.pack.rounds.last.id != round.model.id

      val newStage =
        if (hasMoreQuestions) {
          round
        } else if (hasMoreRounds) {
          val newRound = state.pack.rounds(state.pack.rounds.indexWhere(_.id == round.model.id) + 1)
          Round(round.stage, newRound, Set(), round.activePlayer, round.previousAnswers)
        } else {
          Round(round.stage, state.pack.rounds.head, Set(), round.activePlayer, round.previousAnswers)
        }

      for {
        randomQuestion <- randomQuestionOnTimeout(newStage)
        (cdId, cd) <- ctx.setCountdown(ctx.config.questionSelectionTimeout)(
          _ ! GameCommand.SelectQuestion(newStage.activePlayer, randomQuestion)
        )
        stageWithCd = newStage.copy(stage = Idle(cdId))
        _ <- ctx.broadcast(GameEvent.StageUpdated(stageWithCd.toSnapshot))
      } yield state
        .withStage(stageWithCd)
        .withCd(cdId, cd)
  }

  private def randomQuestionOnTimeout(round: Round): URIO[Random, String] = {
    val availableQuestions = round.model.themes.flatMap(_.questions).filterNot(q => round.takenQuestions.contains(q.id))
    zio.random
      .nextIntBounded(availableQuestions.length)
      .map(availableQuestions)
      .map(_.id)
  }
}
