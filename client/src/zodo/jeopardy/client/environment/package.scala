package zodo.jeopardy.client

import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.logging.Logging
import zio.random.Random
import zio.system.System
import zio.{Has, RIO}
import zodo.jeopardy.client.AppConfig.ServerConfig
import zodo.jeopardy.client.environment.DefaultActorSystem.HasActorSystem
import zodo.jeopardy.model.GameConfig

package object environment {
  type AppEnv = Clock
    with Console
    with System
    with Random
    with Blocking
    with HasActorSystem
    with Logging
    with Has[GameConfig]
    with Has[ServerConfig]
  type AppTask[A] = RIO[AppEnv, A]
}
