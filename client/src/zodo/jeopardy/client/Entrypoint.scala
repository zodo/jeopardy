package zodo.jeopardy.client

import cats.effect.{ExitCode => CatsExitCode, _}
import korolev.http4s
import korolev.state.javaSerialization._
import korolev.zio.zioEffectInstance
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.staticcontent.{FileService, fileService}
import zio.blocking.Blocking
import zio.interop.catz._
import zio.logging.{LogLevel, Logging}
import zio.{App, RIO, URIO, ZEnv, ZIO, ExitCode => ZExitCode}
import zodo.jeopardy.client.environment.{AppEnv, AppTask, DefaultActorSystem}
import zodo.jeopardy.client.views.KorolevService

import scala.concurrent.ExecutionContext

object Entrypoint extends App {

  private val fileRouteM: AppTask[HttpRoutes[AppTask]] = RIO.concurrentEffectWith {
    implicit ce: ConcurrentEffect[AppTask] =>
      for {
        blockingExecutor <- ZIO.access[Blocking](_.get.blockingExecutor)
        blocker = Blocker.liftExecutionContext(blockingExecutor.asEC)
      } yield fileService(FileService.Config("/tmp/korolev", blocker))
  }

  private val appRouteM: AppTask[HttpRoutes[AppTask]] = ZIO.runtime[AppEnv].flatMap { runtime =>
    implicit val ec: ExecutionContext = runtime.platform.executor.asEC
    implicit val effect = zioEffectInstance[AppEnv, Throwable](runtime)(identity)(identity)
    for {
      config <- new KorolevService().configM
      route <- RIO.concurrentEffectWith { implicit ce: ConcurrentEffect[AppTask] =>
        ZIO(http4s.http4sKorolevService(config))
      }
    } yield route
  }

  private val program = for {
    fileRoute <- fileRouteM
    appRoute <- appRouteM

    httpApp = Router[AppTask](
      "/" -> appRoute,
      "/media" -> fileRoute
    ).orNotFound

    _ <- ZIO.runtime[AppEnv].flatMap { implicit rts =>
      BlazeServerBuilder
        .apply[AppTask](rts.platform.executor.asEC)
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp(httpApp)
        .serve
        .compile[AppTask, AppTask, CatsExitCode]
        .drain
    }
  } yield ()

  override def run(args: List[String]): URIO[ZEnv, ZExitCode] = {
    val env = ZEnv.live ++ DefaultActorSystem.live >+> Logging.console(
      logLevel = LogLevel.Debug
    )

    program.provideLayer(env).exitCode
  }
}
