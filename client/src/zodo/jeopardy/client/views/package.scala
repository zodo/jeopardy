package zodo.jeopardy.client
import korolev.Context
import levsha.Document
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.events.ClientEvent
import zodo.jeopardy.model.PackMetaInfo
import zodo.jeopardy.model.PackMetaInfo.MediaMapping

import java.net.URLEncoder

package object views {
  type DocumentNode = Document.Node[Context.Binding[AppTask, ViewState, ClientEvent]]

  object Urls {
    def mediaUrl(packMetaInfo: PackMetaInfo, link: String) = {
      if (link.startsWith("@")) {
        val fileName = link.drop(1)

        findMapping(packMetaInfo.mediaMapping, fileName) match {
          case Some(targetFileName) => s"/pack/${packMetaInfo.hash}/media/$targetFileName"
          case None                 => s"/pack/404/$link"
        }
      } else {
        link
      }
    }

    private def findMapping(mapping: MediaMapping, fileName: String): Option[String] = {
      lazy val encoded = URLEncoder.encode(fileName, "UTF-8").replace("+", "%20")

      lazy val dotnet40encoded = encoded
        .replace("%23", "#")
        .replace("%5B", "[")
        .replace("%5D", "]")
        .replace("%21", "!")
        .replace("%27", "'")
        .replace("%28", "(")
        .replace("%29", ")")
        .replace("%2A", "*")

      lazy val additionalDotnet40encoded = dotnet40encoded
        .replace("%2C", ",")

      val get = mapping.entries.get _

      get(fileName)
        .orElse(get(encoded))
        .orElse(get(dotnet40encoded))
        .orElse(get(additionalDotnet40encoded))
        .orElse {
          println(
            s"Cant find mapping for \nfile $fileName\nenc $encoded\nnet45 $dotnet40encoded\nadd $additionalDotnet40encoded"
          )
          None
        }
    }
  }

}
