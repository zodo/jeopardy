package zodo.jeopardy.actors
import zio.{Fiber, Has}
import zio.clock.Clock
import zio.logging.Logging
import zio.random.Random
import zodo.jeopardy.model.GameConfig

package object game {
  type Env = Logging with Random with Clock with Has[GameConfig]
  type CountdownFiber = Fiber.Runtime[Throwable, Unit]
  type CountdownId = String
}
