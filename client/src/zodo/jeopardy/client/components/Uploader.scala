package zodo.jeopardy.client.components

import korolev.Component
import korolev.Context.FileHandler
import korolev.data.Bytes
import korolev.effect.Effect
import korolev.effect.io.FileIO
import korolev.state.javaSerialization._
import zio.{Task, ZIO}
import zodo.jeopardy.client.AppConfig.ServerConfig
import zodo.jeopardy.client.components.Uploader.UploadStage._
import zodo.jeopardy.client.components.Uploader.{FileUploaded, State}
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.model.{PackMetaInfo, PackModel}
import zodo.jeopardy.service.FileOperations

import java.nio.file.Paths
import scala.util.Try

class Uploader(implicit eff: Effect[AppTask])
    extends Component[AppTask, State, Unit, FileUploaded](State(Waiting, None)) {

  import context._
  import levsha.dsl._
  import html._

  private val fileInput = elementId()

  def render(props: Unit, state: State) = optimize {
    div(
      clazz := "nes-container with-title is-centered",
      p(
        clazz := "title",
        "Upload pack"
      ),
      form(
        label(
          clazz := "nes-btn",
          span(
            "Select file"
          ),
          input(
            `type` := "file",
            accept := ".siq",
            fileInput
          )
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
          `type` := "submit",
          clazz := "nes-btn is-success",
          "Upload",
          state.stage match {
            case Waiting | UnpackError(_) => void
            case _                        => disabled
          },
          event("click")(onUploadClick)
        )
      )
    )
  }

  private def onUploadClick(access: Access) = {

    def fetchFile(handler: FileHandler, data: korolev.effect.Stream[AppTask, Bytes]) = {
      for {
        config <- ZIO.service[ServerConfig]
        size = handler.size
        path = Paths.get(config.packContentPath, handler.fileName)
        result <- data
          .over(0L) {
            case (acc, chunk) =>
              val loaded = chunk.fold(acc)(_.length + acc)
              access
                .transition(_.copy(percentage = Try((loaded / size * 100).toInt).toOption))
                .as(loaded)
          }
          .to(FileIO.write(path))
          .as(path)
      } yield result
    }

    (for {
      files           <- access.downloadFilesAsStream(fileInput)
      _               <- access.transition(_.copy(stage = Uploading, percentage = Some(0)))
      (handler, data) <- Task(files.head)

      downloadedFilePath <- fetchFile(handler, data)

      _        <- access.transition(_.copy(stage = Unpacking, percentage = None))
      fileInfo <- FileOperations.unpackFile(downloadedFilePath)
      _        <- access.publish(FileUploaded(fileInfo.meta, fileInfo.model))
    } yield ())
      .catchAll(err => access.transition(_ => State(UnpackError(err.toString), None)))
  }
}

object Uploader {

  case class FileUploaded(meta: PackMetaInfo, model: PackModel.Pack)

  case class State(stage: UploadStage, percentage: Option[Int])

  sealed trait UploadStage
  object UploadStage {
    case object Waiting extends UploadStage
    case object Uploading extends UploadStage
    case object Unpacking extends UploadStage
    case class UnpackError(message: String) extends UploadStage
  }
}
