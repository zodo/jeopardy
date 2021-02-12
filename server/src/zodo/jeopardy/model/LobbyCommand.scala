package zodo.jeopardy.model

sealed trait LobbyCommand[+_]
object LobbyCommand {
  case class NewGame(hash: String, pack: PackModel.Pack) extends LobbyCommand[GameEntry]
  case class GetGameEntry(id: String) extends LobbyCommand[Option[GameEntry]]
  case class EndGame(id: String) extends LobbyCommand[Unit]
  case class AddPlayer(id: PlayerId) extends LobbyCommand[Either[String, Unit]]
  case class RemovePlayer(id: PlayerId) extends LobbyCommand[Unit]
}
