package zodo.jeopardy.client.environment

import zio.{Has, RIO, ZIO, ZLayer}
import zio.actors.ActorSystem

object DefaultActorSystem {

  type HasActorSystem = Has[DefaultActorSystem.Service]

  trait Service extends Serializable {
    val actorSystem: ActorSystem
  }

  val live: ZLayer[Any, Throwable, HasActorSystem] = ActorSystem("default")
    .map(as =>
      new Service {
        override val actorSystem: ActorSystem = as
      }
    )
    .toLayer

  val system: RIO[HasActorSystem, ActorSystem] = ZIO.access(_.get.actorSystem)
}
