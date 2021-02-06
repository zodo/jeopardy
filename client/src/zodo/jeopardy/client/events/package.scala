package zodo.jeopardy.client

import korolev.Context
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.views.ViewState

package object events {
  type Access = Context.BaseAccess[AppTask, ViewState, ClientEvent]
}
