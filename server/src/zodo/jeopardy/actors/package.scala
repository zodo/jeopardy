package zodo.jeopardy
import zio.actors.ActorRef
import zodo.jeopardy.model.{GameCommand, GameEvent, LobbyCommand}

package object actors {
  type GameActorRef = ActorRef[GameCommand]
  type PlayerActorRef = ActorRef[GameEvent]
  type LobbyActorRef = ActorRef[LobbyCommand]

}
