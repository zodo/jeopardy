package zodo.jeopardy.model

import upickle.default.{macroRW, ReadWriter => RW}
import zodo.jeopardy.model.PackMetaInfo.MediaMapping
import zodo.jeopardy.model.PackMetaInfo.MediaMapping.{SourceFileName, TargetFileName}

case class PackMetaInfo(hash: String, mediaMapping: MediaMapping)

object PackMetaInfo {
  case class MediaMapping(entries: Map[SourceFileName, TargetFileName])
  object MediaMapping {
    type SourceFileName = String
    type TargetFileName = String

    implicit val mappingRw: RW[MediaMapping] = macroRW
  }
}
