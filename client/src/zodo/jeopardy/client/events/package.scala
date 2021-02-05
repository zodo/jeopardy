package zodo.jeopardy.client

import korolev.Context
import zodo.jeopardy.client.environment.AppTask
import zodo.jeopardy.client.views.RootState

package object events {
  type Access = Context.BaseAccess[AppTask, RootState, ClientEvent]
}
