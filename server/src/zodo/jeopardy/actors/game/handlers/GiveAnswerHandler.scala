package zodo.jeopardy.actors.game.handlers

import zodo.jeopardy.actors.game.State
import zodo.jeopardy.actors.game.State.Stage
import zodo.jeopardy.actors.game.State.Stage.RoundStage
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{AwaitingAnswer, Question}
import zodo.jeopardy.model.GameCommand.{GiveAnswer, ShowAnswer}
import zodo.jeopardy.model.{GameEvent, PackModel}

object GiveAnswerHandler extends Handler[GiveAnswer] {
  override def process(m: GiveAnswer, ctx: HandlerContext) = {
    case state @ RoundStage(aa: AwaitingAnswer, r) =>
      val question = aa.questionStage.model
      if (isCorrect(question.answers, m.answer)) {
        handleCorrectAnswer(m, ctx, state, aa, r, question)
      } else {
        handleIncorrectAnswer(m, ctx, state, aa, question)
      }
  }

  private def isCorrect(correctAnswer: PackModel.Answers, actualAnswer: String) = {
    val sanitize = (_: String).replaceAll("[^\\p{L}\\p{N}]+", "").toLowerCase

    correctAnswer.correct.map(sanitize).contains(sanitize(actualAnswer))
  }

  private def handleCorrectAnswer(
    m: GiveAnswer,
    ctx: HandlerContext,
    state: State,
    aa: AwaitingAnswer,
    r: Stage.Round,
    question: PackModel.Question
  ) = {
    val newState = state
      .withPlayerScore(m.playerId, _ + question.price)
      .withStage(
        r.copy(
          takenQuestions = r.takenQuestions + question.id,
          activePlayer = m.playerId
        )
      )
      .withoutCd(aa.cdId)
    for {
      _ <- ctx.stoppedCountdown(aa.cdId)
      _ <- ctx.broadcast(GameEvent.PlayerGaveAnswer(m.playerId, m.answer, isCorrect = true))
      _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
      _ <- ctx.self ! ShowAnswer(question)
    } yield newState
  }

  private def handleIncorrectAnswer(
    m: GiveAnswer,
    ctx: HandlerContext,
    state: State,
    aa: AwaitingAnswer,
    question: PackModel.Question
  ) = {
    for {
      _          <- ctx.stoppedCountdown(aa.cdId)
      (cdId, cd) <- ctx.setCountdown(ctx.config.answerTimeout, aa.questionSecondsPassed)(_ ! ShowAnswer(question))
      newState = state
        .withPlayerScore(m.playerId, _ - question.price)
        .withCd(cdId, cd)
        .withoutCd(aa.cdId)
        .withRoundStage(Question(question, cdId))
      _ <- ctx.broadcast(GameEvent.PlayerGaveAnswer(m.playerId, m.answer, isCorrect = false))
      _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
      _ <- ctx.broadcast(GameEvent.StageUpdated(newState.stage.toSnapshot))
    } yield newState
  }
}
