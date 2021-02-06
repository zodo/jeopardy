package zodo.jeopardy.model
import zodo.jeopardy.actors.GameActorRef

case class GameEntry(id: String, packId: String, game: GameActorRef)
