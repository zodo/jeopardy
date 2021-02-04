package zodo.jeopardy.client

import korolev.effect.Effect
import korolev.{Context, Extension, Qsid}
import zio._
import zio.actors.Actor.Stateful
import zio.actors._
import zio.console.{Console, putStrLn}

final class EventsMediator(implicit eff: Effect[EnvTask]) extends Extension[EnvTask, AppState, ClientEvent] {

  type Access = Context.BaseAccess[EnvTask, AppState, ClientEvent]

  val proxy = new Stateful[Console, Unit, ParametrizedClientEvent] {

    override def receive[A](
      state: Unit,
      msg: ParametrizedClientEvent[A],
      context: actors.Context
    ): RIO[Console, (Unit, A)] =
      msg match {
        case s: ClientEvent =>
          putStrLn(s"\t in actor $s").as((), ())
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

  def setup(access: Access): EnvTask[Extension.Handlers[EnvTask, AppState, ClientEvent]] = {
    val value = for {
      session <- access.sessionId
      actorSystem <- DefaultActorSystem.system
      _ <- putStrLn(s"Using actor system: $actorSystem")
      actor <- actorSystem.make("unique-proxy", actors.Supervisor.none, (), proxy)
      _ <- sendMessage(actor, ClientEvent.EnterGame("test1", "test2"))
      _ <- sendMessage(actor, ClientEvent.EnterGame("test3", "test4"))
    } yield Extension.Handlers[EnvTask, AppState, ClientEvent](
      onDestroy = () => actor.stop.unit,
      onMessage = clientEvent => {
        for {
          _ <- sendMessage(actor, clientEvent)
          mailbox <- actor.stop
          _ <- putStrLn(s"Mailbox: $mailbox")
        } yield ()

      }
    )
    value
  }
}
