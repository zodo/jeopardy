package zodo.jeopardy

import zio._

package object client {
    type EnvTask[A] = RIO[ZEnv, A]
}
