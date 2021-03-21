package zodo.jeopardy.model
import zodo.jeopardy.actors.GameActorRef

case class GameEntry(id: String, packMetaInfo: PackMetaInfo, game: GameActorRef)
