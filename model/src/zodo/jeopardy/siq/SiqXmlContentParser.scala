package zodo.jeopardy.siq

import zodo.jeopardy.model.PackModel
import scala.xml._

object SiqXmlContentParser {

  def convert(xml: NodeSeq): PackModel.Pack = PackModel.Pack(
    (xml \ "rounds" \\ "round").map(r =>
      PackModel.Round(
        r \@ "name",
        (r \ "themes" \\ "theme").map(t =>
          PackModel.Theme(
            t \@ "name",
            (t \ "questions" \\ "question").map(mapQuestion(t \@ "name"))
          )
        ),
        if ((r \@ "type") == "final") PackModel.RoundType.Final else PackModel.RoundType.Standard
      )
    )
  )

  private[siq] def mapQuestion(theme: String)(q: NodeSeq): PackModel.Question = {
    val atoms = q \ "scenario" \\ "atom"
    val markerIndex = atoms.indexWhere(a => (a \@ "type") == "marker")
    val (questions, mediaAnswers) = if (markerIndex > 0) {
      (atoms.take(markerIndex).map(mapAtom), atoms.drop(markerIndex + 1).map(mapAtom))
    } else {
      (atoms.map(mapAtom), Nil)
    }

    PackModel.Question(
      questions,
      PackModel.Answers(
        (q \ "right" \\ "answer").map(_.text),
        (q \ "wrong" \\ "answer").map(_.text),
        mediaAnswers
      ),
      (q \@ "price").toInt,
      theme
    )
  }

  private def mapAtom(a: NodeSeq): PackModel.Fragment = {
    val time = (a \\ "@time").headOption.flatMap(_.text.toIntOption)

    a \@ "type" match {
      case "say" | "text" | "" => PackModel.Fragment.Text(a.text)
      case "image"             => PackModel.Fragment.Image(a.text)
      case "voice"             => PackModel.Fragment.Audio(a.text, time)
      case "video"             => PackModel.Fragment.Video(a.text, time)
    }
  }
}
