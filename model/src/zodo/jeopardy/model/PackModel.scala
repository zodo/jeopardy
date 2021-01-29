package zodo.jeopardy.model

object PackModel {

  case class Pack(rounds: Seq[Round])

  case class Round(name: String, themes: Seq[Theme], typ: RoundType)

  sealed trait RoundType
  object RoundType {
    case object Standard extends RoundType
    case object Final extends RoundType
  }

  case class Theme(name: String, questions: Seq[Question])

  case class Question(fragments: Seq[Fragment], answers: Answers, price: Long)

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
