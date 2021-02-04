package zodo.jeopardy

import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.system.System
import zodo.jeopardy.client.DefaultActorSystem.HasDefaultActorSystem

package object client {
  type CustomEnv = Clock with Console with System with Random with Blocking with HasDefaultActorSystem
  type EnvTask[A] = RIO[CustomEnv, A]
}
