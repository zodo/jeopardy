package zodo.jeopardy.actors.game.handlers

import zodo.jeopardy.actors.game.State
import zodo.jeopardy.actors.game.State.Stage.Round
import zodo.jeopardy.actors.game.State.Stage.RoundStage.Idle
import zodo.jeopardy.model.{GameCommand, GameEvent}

object ReturnToRoundHandler extends Handler[Unit] {
  override def process(m: Unit, ctx: HandlerContext) = {
    case state @ State(round: Round) =>
      val hasMoreQuestions = round.model.themes.flatMap(_.questions).size > round.takenQuestions.size
      val hasMoreRounds = state.pack.rounds.last.id != round.model.id

      for {
        (cdId, cd) <- ctx.setCountdown(ctx.config.questionSelectionTimeout)(_ ! GameCommand.ChooseRandomQuestion)
        newStage =
          if (hasMoreQuestions) {
            round.copy(stage = Idle(cdId))
          } else if (hasMoreRounds) {
            val newRound = state.pack.rounds(state.pack.rounds.indexWhere(_.id == round.model.id) + 1)
            Round(Idle(cdId), newRound, Set(), round.activePlayer)
          } else {
            Round(Idle(cdId), state.pack.rounds.head, Set(), round.activePlayer)
          }

        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
      } yield state
        .withStage(newStage)
        .withCd(cdId, cd)
  }
}
