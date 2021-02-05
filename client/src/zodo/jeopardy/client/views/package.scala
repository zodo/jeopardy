package zodo.jeopardy.client
import korolev.Context
import levsha.Document
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

package object views {
  type DocumentNode = Document.Node[Context.Binding[AppTask, RootState, ClientEvent]]
}
