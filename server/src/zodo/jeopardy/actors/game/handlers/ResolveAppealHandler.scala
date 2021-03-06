package zodo.jeopardy.actors.game.handlers

import zio.duration.durationInt
import zio.logging.log
import zodo.jeopardy.actors.game.State.Stage.RoundStage.{Appeal, AppealResult, Idle}
import zodo.jeopardy.actors.game.State.Stage.{AnswersSummary, Round, RoundStage}
import zodo.jeopardy.model.GameCommand.ResolveAppeal
import zodo.jeopardy.model.{GameCommand, GameEvent}

object ResolveAppealHandler extends Handler[ResolveAppeal] {
  override def process(m: ResolveAppeal, ctx: HandlerContext) = {
    case state @ RoundStage(a: Appeal, round: Round) =>
      def approveAppeal = {
        val rollbackScores = round.previousAnswers.rollbackScores(a.playerId)
        val newState = state
          .withPlayers(player => {
            val scoreDiff = if (a.playerId == player.id) {
              a.model.price - rollbackScores.getOrElse(player.id, 0)
            } else {
              -rollbackScores.getOrElse(player.id, 0)
            }
            player.copy(score = player.score + scoreDiff)
          })
          .withStage(
            round.copy(
              stage = AppealResult(resolution = true),
              activePlayer = a.playerId,
              previousAnswers = AnswersSummary(None, Seq())
            )
          )

        for {
          _ <- log.debug(s"Appeal approved")
          _ <- ctx.stoppedCountdown(a.cdId)
          _ <- ctx.broadcast(GameEvent.StageUpdated(newState.stage.toSnapshot))
          _ <- ctx.broadcast(GameEvent.PlayersUpdated(newState.players.map(_.toMessage)), newState)
          _ <- (ctx.self ! GameCommand.ReturnToRound).delay(3.seconds).fork
        } yield newState.withoutCd(a.cdId)
      }

      def rejectAppeal = {
        val newStage = round.copy(
          stage = AppealResult(resolution = false),
          previousAnswers = round.previousAnswers.copy(triedToAppeal = round.previousAnswers.triedToAppeal + a.playerId)
        )

        for {
          _ <- log.debug(s"Appeal rejected")
          _ <- ctx.stoppedCountdown(a.cdId)
          _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
          _ <- (ctx.self ! GameCommand.ReturnToRound).delay(3.seconds).fork
        } yield state.withStage(newStage).withoutCd(a.cdId)
      }

      val resolutions = a.resolutions.updated(m.playerId, m.resolution)
      val playersCount = state.players.filterNot(_.disconnected).size

      val (agree, disagree) = resolutions.partition(_._2)

      val agreeLimit = (playersCount - 1) / 2 + 1
      val disagreeLimit = playersCount / 2

      if (m.playerId == a.playerId) {
        log
          .debug(s"Can't resolve, same player")
          .as(state)
      } else if (agree.size >= agreeLimit) {
        approveAppeal
      } else if (disagree.size >= disagreeLimit) {
        rejectAppeal
      } else {
        val newStage = round.copy(
          stage = a.copy(resolutions = resolutions)
        )

        for {
          _ <- log.debug(s"New resolutions: $resolutions")
          _ <- ctx.broadcast(GameEvent.StageUpdated(newStage.toSnapshot))
        } yield state.withStage(newStage)
      }
  }
}
