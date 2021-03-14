package zodo.jeopardy.client
import korolev.Context
import levsha.Document
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent

import java.net.URLEncoder

package object views {
  type DocumentNode = Document.Node[Context.Binding[AppTask, ViewState, ClientEvent]]

  object Urls {
    def imageUrl(hash: String, link: String) = resourceUrl(hash, "Images", link)

    def videoUrl(hash: String, link: String) = resourceUrl(hash, "Video", link)

    def audioUrl(hash: String, link: String) = resourceUrl(hash, "Audio", link)

    private def resourceUrl(hash: String, dir: String, link: String) = {
      if (link.startsWith("@")) {
        val preparedLink = URLEncoder.encode(link.drop(1), "UTF-8").replace("+", "%20")
        s"/media/$hash/$dir/$preparedLink"
      } else {
        link
      }
    }
  }

}
