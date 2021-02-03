package zodo.jeopardy.client

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import korolev.akka._
import korolev.effect.Effect
import korolev.zio.zioEffectInstance
import zio.Runtime
import korolev.state.javaSerialization._

import scala.concurrent.ExecutionContext.Implicits.global

object Entrypoint extends App {
  val runtime = Runtime.default
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
