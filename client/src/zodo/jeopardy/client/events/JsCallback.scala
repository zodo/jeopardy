package zodo.jeopardy.client.events

sealed trait JsCallback {
  val name: String

  def call(arg: String) = s"Korolev.invokeCallback('$name', '$arg')"
}

object JsCallback {
  object MediaFinished extends JsCallback {
    val name = "mediaFinished"
  }
}
