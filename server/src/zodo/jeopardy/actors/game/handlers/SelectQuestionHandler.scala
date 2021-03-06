package zodo.jeopardy.actors.game.handlers

import zio._
import zio.duration.durationInt
import zodo.jeopardy.actors.game.State.Stage.{AnswersSummary, RoundStage}
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Idle, Question}
import zodo.jeopardy.model.GameCommand.{SelectQuestion, ShowAnswer}
import zodo.jeopardy.model.{GameCommand, GameEvent}

object SelectQuestionHandler extends Handler[SelectQuestion] {
  override def process(m: SelectQuestion, ctx: HandlerContext) = {
    case state @ RoundStage(i: Idle, round) =>
      val canFail = for {
        question <- ZIO.fromOption(round.model.themes.flatMap(_.questions).find(_.id == m.questionId))
        _        <- ZIO.fail(()).when(round.takenQuestions.contains(m.questionId))
        _        <- ZIO.fail(()).when(round.activePlayer != m.playerId)
        players = state.players.filterNot(_.disconnected).map(_.id).toSet
        _ <- ZIO
          .foreachPar_(players)(p => ctx.self ! GameCommand.FinishQuestion(p, m.questionId, force = true))
          .delay(ctx.config.questionReadingTimeout.seconds)
          .fork
        newStage = round.copy(
          stage = Question(question, players.map(_ -> 0).toMap),
          previousAnswers = AnswersSummary(Some(question), Seq())
        )
        _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
        _ <- ctx.stoppedCountdown(i.cdId)
      } yield state
        .withoutCd(i.cdId)
        .withStage(newStage)

      canFail.orElseSucceed(state)
  }
}
