package zodo.jeopardy.service

import net.lingala.zip4j.ZipFile
import zio._
import zio.blocking._
import zio.stream.{ZSink, ZStream}
import zodo.jeopardy.model.{PackMetaInfo, PackModel}
import zodo.jeopardy.siq.SiqXmlContentParser

import java.io.File
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.UUID
import scala.jdk.Accumulator
import scala.jdk.StreamConverters.StreamHasToScala
import scala.xml.XML
import upickle.default._
import zodo.jeopardy.model.PackMetaInfo.MediaMapping

object FileOperations {

  private val mediaMappingFile = "media-mapping.json"

  case class FileInfo(meta: PackMetaInfo, model: PackModel.Pack)

  def unpackFile(filePath: Path): RIO[Blocking, FileInfo] = {
    for {
      md5             <- calculateMd5(filePath)
      targetDirectory <- UIO(filePath.getParent.resolve(md5))
      _               <- unpackZip(filePath, targetDirectory).unless(Files.exists(targetDirectory))
      mediaMapping    <- Task(read[MediaMapping](targetDirectory.resolve(mediaMappingFile).toFile))
      pack            <- readAndParsePack(targetDirectory)
    } yield FileInfo(PackMetaInfo(md5, mediaMapping), pack)
  }

  private def calculateMd5(path: Path): RIO[Blocking, String] = {
    val md5HashSink = ZSink
      .foldLeftChunks(MessageDigest.getInstance("MD5")) { (hasher, chunk: Chunk[Byte]) =>
        hasher.update(chunk.toArray)
        hasher
      }
      .map(_.digest().foldLeft("")((acc, byte) => s"$acc${String.format("%02x", byte)}"))

    ZStream.fromFile(path).run(md5HashSink)
  }

  private def unpackZip(zipFile: Path, targetDirectory: Path) = {
    for {
      _       <- effectBlocking(Files.createDirectory(targetDirectory))
      zipFile <- Task(new ZipFile(new File(zipFile.toUri)))
      _       <- effectBlocking(zipFile.extractAll(targetDirectory.toString))
      _       <- Task(generateMediaMapping(targetDirectory))
    } yield ()
  }

  private def readAndParsePack(targetDirectory: Path) = {
    for {
      contentFile <- UIO(targetDirectory.resolve("content.xml"))
      xml         <- Task(XML.loadFile(new File(contentFile.toUri)))
      parsed      <- Task(SiqXmlContentParser.convert(xml))
    } yield parsed
  }

  private def generateMediaMapping(path: Path): Unit = {
    val entries = Seq("Audio", "Video", "Images")
      .flatMap(mediaDirName => {
        val mediaDir = path.resolve(mediaDirName)
        if (Files.exists(mediaDir) && Files.isDirectory(mediaDir)) {
          Files
            .list(mediaDir)
            .toScala(Accumulator)
            .map(file => {
              val sourceName = file.getFileName.toString
              val extension = sourceName.split('.').last
              val newName = UUID.randomUUID().toString + "." + extension

              Files.move(file, file.resolveSibling(newName))
              (sourceName, newName)
            })
        } else {
          Seq()
        }
      })
      .toMap

    val mapping = MediaMapping(entries)
    val mappingPath = path.resolve(mediaMappingFile)

    Files.write(mappingPath, writeToByteArray(mapping, indent = 2))
  }
}
