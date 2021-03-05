package zodo.jeopardy.actors.game

import zio.actors.{Actor, Context}
import zio.logging.log
import zio.{RIO, UIO, ZIO}
import zodo.jeopardy.actors.LobbyActorRef
import zodo.jeopardy.actors.game.State.Stage.BeforeStart
import zodo.jeopardy.actors.game.handlers._
import zodo.jeopardy.model.GameCommand._
import zodo.jeopardy.model.StageSnapshot.ReadyForHit
import zodo.jeopardy.model.{GameCommand, GameConfig, PackModel}

object GameActor {

  def initState(id: String, pack: PackModel.Pack): State = State(id, pack, Seq(), BeforeStart, Map())

  def handler(lobby: LobbyActorRef): Actor.Stateful[Env, State, GameCommand] =
    new Actor.Stateful[Env, State, GameCommand] {
      override def receive[A](state: State, msg: GameCommand[A], context: Context): RIO[Env, (State, A)] = {
        for {
          _      <- log.debug(s"GameActor <- $msg")
          config <- ZIO.service[GameConfig]
          self   <- context.self[GameCommand]
          ctx = new HandlerContext.Default(lobby, config, self, state)
          handler = msg match {
            case m: AddPlayer        => AddPlayerHandler.process(m, ctx)
            case m: DisconnectPlayer => DisconnectPlayerHandler.process(m, ctx)
            case Start               => StartHandler.process((), ctx)
            case m: SelectQuestion   => SelectQuestionHandler.process(m, ctx)
            case m: FinishQuestion   => FinishQuestionHandler.process(m, ctx)
            case m: HitButton        => HitButtonHandler.process(m, ctx)
            case m: GiveAnswer       => GiveAnswerHandler.process(m, ctx)
            case m: ShowAnswer       => ShowAnswerHandler.process(m, ctx)
            case ReturnToRound       => ReturnToRoundHandler.process((), ctx)
            case m: TickCountdown    => TickCountdownHandler.process(m, ctx)
          }
          newState <- handler.applyOrElse(state, (_: State) => UIO(state))
        } yield newState -> ().asInstanceOf[A]
      }
    }
}
