package zodo.jeopardy.model

case class GameConfig(
  questionSelectionTimeout: Int,
  hitTheButtonTimeout: Int,
  answerTimeout: Int,
  questionReadingTimeout: Int
)
