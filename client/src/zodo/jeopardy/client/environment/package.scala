package zodo.jeopardy.client

import zio.RIO
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.logging.Logging
import zio.random.Random
import zio.system.System
import zodo.jeopardy.client.environment.DefaultActorSystem.HasActorSystem

package object environment {
  type AppEnv = Clock with Console with System with Random with Blocking with HasActorSystem with Logging
  type AppTask[A] = RIO[AppEnv, A]
}
