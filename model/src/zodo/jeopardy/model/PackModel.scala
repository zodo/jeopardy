package zodo.jeopardy.model

import java.util.UUID

object PackModel {

  trait WithId {
    val id: String = UUID.randomUUID().toString
  }

  case class Pack(rounds: Seq[Round])

  case class Round(name: String, themes: Seq[Theme], typ: RoundType) extends WithId

  sealed trait RoundType

  object RoundType {
    case object Standard extends RoundType
    case object Final extends RoundType
  }

  case class Theme(name: String, questions: Seq[Question]) extends WithId

  case class Question(fragments: Seq[Fragment], answers: Answers, price: Long) extends WithId

  case class Answers(
    correct: Seq[String],
    incorrect: Seq[String],
    additional: Seq[Fragment]
  )

  sealed trait Fragment

  object Fragment {
    case class Text(value: String) extends Fragment
    case class Image(url: String) extends Fragment
    case class Audio(url: String) extends Fragment
    case class Video(url: String) extends Fragment
  }
}
