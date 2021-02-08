package zodo.jeopardy.client.events
import zio.RIO
import zio.actors.{Actor, ActorRef, Context}
import zodo.jeopardy.client.environment.AppEnv
import zodo.jeopardy.client.views.ViewState
import zodo.jeopardy.client.views.ViewState.InGame

object ViewStateUpdaterActor {

  type ViewStateUpdaterActorRef = ActorRef[Message]

  def handler(access: Access): Actor.Stateful[AppEnv, Unit, Message] = new Actor.Stateful[AppEnv, Unit, Message] {
    override def receive[A](state: Unit, msg: Message[A], context: Context): RIO[AppEnv, (Unit, A)] = msg match {
      case Update(f) => access.syncTransition(f).as(() -> ().asInstanceOf[A])
      case UpdateGame(f) =>
        access
          .syncTransition {
            case s: InGame => f(s)
            case s         => s
          }
          .as(() -> ().asInstanceOf[A])
    }
  }

  sealed trait Message[+_]
  case class Update(f: ViewState => ViewState) extends Message[Unit]
  case class UpdateGame(f: InGame => InGame) extends Message[Unit]
}
