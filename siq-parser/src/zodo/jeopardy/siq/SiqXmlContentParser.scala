package zodo.jeopardy.siq

import com.jeopardy.siq.xml._
import zodo.jeopardy.model.PackModel

import scala.xml.NodeSeq

object SiqXmlContentParser {

  def parse(xml: NodeSeq): PackModel.Pack = {
    val parsed = scalaxb.fromXML[Package](xml)
    convert(parsed)
  }

  private def convert(p: Package): PackModel.Pack = PackModel.Pack(
    p.rounds.round.map(r =>
      PackModel.Round(
        r.name,
        r.themes.theme.map(t =>
          PackModel.Theme(
            t.name,
            t.questions.question.map(mapQuestion)
          )
        ),
        if (r.typeValue.contains(Final)) PackModel.RoundType.Final else PackModel.RoundType.Standard
      )
    )
  )

  private[siq] def mapQuestion(q: Question): PackModel.Question = {
    val atoms = q.scenario.atom
    val markerIndex = atoms.indexWhere(_.typeValue.contains(Marker))
    val (questions, mediaAnswers) = if (markerIndex > 0) {
      (atoms.take(markerIndex).map(mapAtom), atoms.drop(markerIndex + 1).map(mapAtom))
    } else {
      (atoms.map(mapAtom), Nil)
    }

    PackModel.Question(
      questions,
      PackModel.Answers(
        q.right.answer,
        q.wrong.map(_.answer).getOrElse(Nil),
        mediaAnswers
      ),
      q.price
    )
  }

  private def mapAtom(a: Atom): PackModel.Fragment = a.typeValue match {
    case Some(Say) | Some(Text) | None => PackModel.Fragment.Text(a.value)
    case Some(Image)                   => PackModel.Fragment.Image(a.value)
    case Some(Voice)                   => PackModel.Fragment.Audio(a.value)
    case Some(Video)                   => PackModel.Fragment.Video(a.value)
  }
}
