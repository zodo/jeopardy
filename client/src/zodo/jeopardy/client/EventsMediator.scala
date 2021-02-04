package zodo.jeopardy.client

import korolev.effect.Effect
import korolev.{Context, Extension}
import zio._
import zio.actors.Actor.Stateful
import zio.actors._
import zio.console.{Console, putStrLn}

final class EventsMediator(implicit eff: Effect[AppTask]) extends Extension[AppTask, AppState, ClientEvent] {

  type Access = Context.BaseAccess[AppTask, AppState, ClientEvent]

  val proxy = new Stateful[Console, Unit, ParametrizedClientEvent] {

    override def receive[A](
      state: Unit,
      msg: ParametrizedClientEvent[A],
      context: actors.Context
    ): RIO[Console, (Unit, A)] =
      msg match {
        case s: ClientEvent =>
          putStrLn(s"\t in actor $s").as((), ().asInstanceOf[A])
      }
  }

  private def sendMessage[A, F[+_]](ref: ActorRef[F], message: F[A]): RIO[Console, A] = {
    for {
      path <- ref.path
      _ <- putStrLn(s"${message} -> ActorRef(${ref.toString}, $path)")
      response <- ref ? message
      _ <- putStrLn(s"ActorRef(${ref.toString}, $path) -> $response")
    } yield response
  }

  def setup(access: Access): AppTask[Extension.Handlers[AppTask, AppState, ClientEvent]] = {
    val value = for {
      session <- access.sessionId
      actorSystem <- DefaultActorSystem.system
      _ <- putStrLn(s"Using actor system: $actorSystem")
      actor <- actorSystem.make(s"proxy-$session", actors.Supervisor.none, (), proxy)
    } yield Extension.Handlers[AppTask, AppState, ClientEvent](
      onDestroy = () => actor.stop.unit,
      onMessage = clientEvent => {
        for {
          _ <- sendMessage(actor, clientEvent)
        } yield ()

      }
    )
    value
  }
}
