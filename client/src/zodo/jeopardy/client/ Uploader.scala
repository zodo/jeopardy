package zodo.jeopardy.client

import zio.Task
import korolev.effect.io.FileIO
import korolev.zio._
import korolev.state.javaSerialization._

import scala.concurrent.duration.DurationInt
import java.nio.file.Paths
import korolev.effect.Effect
import korolev.Component
import scala.xml.XML
import zodo.jeopardy.siq.SiqXmlContentParser
import zodo.jeopardy.client.Uploader.State

class Uploader(implicit eff: Effect[Task]) extends Component[Task, State, Unit, AppEvent](State(Map.empty, false)) {

  import context._

  import levsha.dsl._
  import html._

  val fileInput = elementId()

  def render(props: Unit, state: State) = optimize {
    div(
      input(`type` := "file", multiple, fileInput),
      ul(
        state.progress.map { case (name, (loaded, total)) =>
          li(s"$name: $loaded / $total")
        }
      ),
      button(
        "Upload",
        when(state.inProgress)(disabled),
        event("click")(onUploadClick)
      )
    )
  }

  def onUploadClick(access: Access) = {

    def showProgress(fileName: String, loaded: Long, total: Long) = access
      .transition { state =>
        val updated = state.progress + ((fileName, (loaded, total)))
        state.copy(progress = updated)
      }

    for {
      files <- access.downloadFilesAsStream(fileInput)
      _ <- access.transition(
        _.copy(progress = files.map(x => (x._1.fileName, (0L, x._1.size))).toMap, inProgress = true)
      )
      downloadedFiles <- Task.collectAll {
        files.map { case (handler, data) =>
          val size = handler.size
          val path = Paths.get(s"/tmp/korolev/${handler.fileName}")
          data
            .over(0L) { case (acc, chunk) =>
              val loaded = chunk.fold(acc)(_.length.toLong + acc)
              Thread.sleep(200)
              showProgress(handler.fileName, loaded, size)
                .map(_ => loaded)
            }
            .to(FileIO.write(path))
            .map(_ => path)
        }
      }
      _ <- access.transition(_.copy(inProgress = false))
      xml <- Task.effect(XML.loadFile(downloadedFiles.head.toAbsolutePath().toString()))
      parsed <- Task.effect(SiqXmlContentParser.convert(xml))
      _ <- access.publish(AppEvent.FileProcessed(parsed))
    } yield ()
  }

}

object Uploader {
  case class State(progress: Map[String, (Long, Long)], inProgress: Boolean)
}
