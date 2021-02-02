package zodo.jeopardy.client

import zodo.jeopardy.model.PackModel

sealed trait AppEvent
object AppEvent {
    case class FileProcessed(hash: String, pack: PackModel.Pack) extends AppEvent
}