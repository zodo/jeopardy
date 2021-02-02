package zodo.jeopardy.client

import korolev.Component
import korolev.Context.FileHandler
import korolev.data.Bytes
import korolev.effect.Effect
import korolev.effect.io.FileIO
import korolev.state.javaSerialization._
import zio.Task
import zodo.jeopardy.client.Uploader.State
import zodo.jeopardy.client.Uploader.UploadStage.{UnpackError, Unpacking, Uploading, Waiting}
import zodo.jeopardy.core.FileOperations

import java.nio.file.Paths
import scala.util.Try

class Uploader(implicit eff: Effect[EnvTask]) extends Component[EnvTask, State, Unit, AppEvent](State(Waiting, None)) {

  import context._
  import levsha.dsl._
  import html._

  val fileInput = elementId()

  def render(props: Unit, state: State) = {
    div(
      input(
        `type` := "file",
        accept := ".siq",
        fileInput
      ),
      state.stage match {
        case Waiting => void
        case Uploading =>
          div(
            p("Uploading "),
            state.percentage.map(per => p(s"${per.toString}%"))
          )
        case Unpacking =>
          div("Unpacking...")
        case UnpackError(message) =>
          div(
            backgroundColor @= "red",
            message
          )
      },
      button(
        "Upload",
        state.stage match {
          case Waiting | UnpackError(_) => void
          case _                        => disabled
        },
        event("click")(onUploadClick)
      )
    )
  }

  private def onUploadClick(access: Access) = {

    def fetchFile(handler: FileHandler, data: korolev.effect.Stream[EnvTask, Bytes]) = {
      val size = handler.size
      val path = Paths.get(s"/tmp/korolev/${handler.fileName}")
      data
        .over(0L) { case (acc, chunk) =>
          val loaded = chunk.fold(acc)(_.length + acc)
          access
            .transition(_.copy(percentage = Try((loaded / size * 100).toInt).toOption))
            .as(loaded)
        }
        .to(FileIO.write(path))
        .as(path)
    }

    (for {
      files <- access.downloadFilesAsStream(fileInput)
      _ <- access.transition(_.copy(stage = Uploading, percentage = Some(0)))
      (handler, data) <- Task(files.head)

      downloadedFilePath <- fetchFile(handler, data)

      _ <- access.transition(_.copy(stage = Unpacking, percentage = None))
      fileInfo <- FileOperations.unpackFile(downloadedFilePath)
      _ <- access.publish(AppEvent.FileProcessed(fileInfo.hash, fileInfo.pack))
    } yield ())
      .catchAll(err => access.transition(_ => State(UnpackError(err.toString), None)))
  }
}

object Uploader {
  case class State(stage: UploadStage, percentage: Option[Int])

  sealed trait UploadStage

  object UploadStage {
    case object Waiting extends UploadStage
    case object Uploading extends UploadStage
    case object Unpacking extends UploadStage
    case class UnpackError(message: String) extends UploadStage
  }
}
