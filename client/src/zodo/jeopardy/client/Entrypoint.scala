package zodo.jeopardy.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import korolev.akka._
import korolev.effect.Effect
import korolev.zio.zioEffectInstance
import zio.{Has, Runtime}
import korolev.state.javaSerialization._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.internal.Platform
import zio.random.Random
import zio.system.System

import scala.concurrent.ExecutionContext.Implicits.global

object Entrypoint extends App {

  val runtime = Runtime(
    Has.allOf[
      Clock.Service,
      Console.Service,
      System.Service,
      Random.Service,
      Blocking.Service,
      DefaultActorSystem.Service
    ](
      Clock.Service.live,
      Console.Service.live,
      System.Service.live,
      Random.Service.live,
      Blocking.Service.live,
      DefaultActorSystem.Service.live
    ),
    Platform.default
  )
  implicit val effect: Effect[EnvTask] = zioEffectInstance(runtime)((e: Throwable) => e)((e: Throwable) => e)
  implicit val actorSystem: ActorSystem = ActorSystem()

  import akka.http.scaladsl.server.Directives._

  val korolevService = akkaHttpService(new KorolevService().config)

  val route = concat(
    pathPrefix("media") {
      getFromBrowseableDirectory("/tmp/korolev")
    },
    korolevService(AkkaHttpServerConfig())
  )

  Http().newServerAt("0.0.0.0", 8080).bind(route)
}
