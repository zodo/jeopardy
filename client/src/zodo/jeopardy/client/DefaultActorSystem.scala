package zodo.jeopardy.client

import zio.actors.ActorSystem
import zio.{Has, Layer, RIO, Task, ZIO, ZLayer}

object DefaultActorSystem {

  type HasDefaultActorSystem = Has[DefaultActorSystem.Service]

  trait Service extends Serializable {
    val actorSystem: Task[ActorSystem]
  }

  object Service {

    val live = new Service {
      val actorSystem: Task[ActorSystem] = ActorSystem("default")
    }
  }

  val live: Layer[Nothing, HasDefaultActorSystem] = ZLayer.succeed(Service.live)

  val system: RIO[HasDefaultActorSystem, ActorSystem] = ZIO.accessM(_.get.actorSystem)
}
