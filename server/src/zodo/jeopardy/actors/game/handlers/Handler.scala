package zodo.jeopardy.actors.game.handlers
import zio.RIO
import zodo.jeopardy.actors.game.{Env, State}

trait Handler[T] {
  def process(message: T, ctx: HandlerContext): PartialFunction[State, RIO[Env, State]]
}
