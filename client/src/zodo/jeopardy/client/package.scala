package zodo.jeopardy

import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.logging.Logging
import zio.random.Random
import zio.system.System
import zodo.jeopardy.client.DefaultActorSystem.HasActorSystem

package object client {
  type AppEnv = Clock with Console with System with Random with Blocking with HasActorSystem with Logging
  type AppTask[A] = RIO[AppEnv, A]
}
