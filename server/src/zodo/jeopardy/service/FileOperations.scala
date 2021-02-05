package zodo.jeopardy.service

import net.lingala.zip4j.ZipFile
import zio._
import zio.blocking._
import zio.stream.{ZSink, ZStream}
import zodo.jeopardy.model.PackModel
import zodo.jeopardy.siq.SiqXmlContentParser

import java.io.File
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import scala.xml.XML

object FileOperations {

  case class FileInfo(hash: String, pack: PackModel.Pack)

  def unpackFile(filePath: Path): RIO[Blocking, FileInfo] = {
    for {
      md5 <- calculateMd5(filePath)

      targetDirectory <- UIO(filePath.getParent.resolve(md5))

      _ <- unpackZip(filePath, targetDirectory).unless(Files.exists(targetDirectory))

      pack <- readAndParsePack(targetDirectory)

    } yield FileInfo(md5, pack)
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
      _ <- effectBlocking(Files.createDirectory(targetDirectory))
      zipFile <- Task(new ZipFile(new File(zipFile.toUri)))
      _ <- effectBlocking(zipFile.extractAll(targetDirectory.toString))
    } yield ()
  }

  private def readAndParsePack(targetDirectory: Path) = {
    for {
      contentFile <- UIO(targetDirectory.resolve("content.xml"))
      xml <- Task(XML.loadFile(new File(contentFile.toUri)))
      parsed <- Task(SiqXmlContentParser.convert(xml))
    } yield parsed
  }
}
