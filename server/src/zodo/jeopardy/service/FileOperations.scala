package zodo.jeopardy.service

import net.lingala.zip4j.ZipFile
import upickle.default._
import zio._
import zio.blocking._
import zio.stream.{ZSink, ZStream}
import zodo.jeopardy.model.PackMetaInfo.MediaMapping
import zodo.jeopardy.model.{PackMetaInfo, PackModel}
import zodo.jeopardy.siq.SiqXmlContentParser

import java.io.File
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.UUID
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.xml.XML

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

  private def unpackZip(zipFile: Path, targetDirectory: Path) = Task {
    Files.createDirectory(targetDirectory)

    val zip = new ZipFile(zipFile.toFile)

    val files = zip.getFileHeaders.asScala
      .map(header => {
        val fileName = header.getFileName

        if (Seq("Audio", "Video", "Images").exists(fileName.startsWith)) {
          // resource file, rename on unpack
          val extension = fileName.split('.').last
          val newName = UUID.randomUUID().toString + "." + extension

          (header, Some(newName))
        } else {
          (header, None)
        }
      })

    val target = targetDirectory.toString

    files.foreach {
      case (header, Some(name)) => zip.extractFile(header, target, "media/" + name)
      case (header, None)       => zip.extractFile(header, target)
    }

    val entries = files.collect {
      case (header, Some(name)) => header.getFileName.split("/").last -> name
    }.toMap

    val mapping = MediaMapping(entries)
    val mappingPath = targetDirectory.resolve(mediaMappingFile)

    Files.write(mappingPath, writeToByteArray(mapping, indent = 2))
  }

  private def readAndParsePack(targetDirectory: Path) = {
    for {
      contentFile <- UIO(targetDirectory.resolve("content.xml"))
      xml         <- Task(XML.loadFile(new File(contentFile.toUri)))
      parsed      <- Task(SiqXmlContentParser.convert(xml))
    } yield parsed
  }
}
