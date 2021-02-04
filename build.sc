import mill._, scalalib._

trait BaseScalaModule extends ScalaModule {
  def scalaVersion = "2.13.4"
}

trait MillTests extends TestModule {
  override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.7")
  def testFrameworks = Seq("utest.runner.Framework")
}

object model extends BaseScalaModule

object `siq-parser` extends BaseScalaModule {

  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-xml:1.3.0"
  )

  override def moduleDeps = Seq(model)

  object test extends Tests with MillTests
}

object core extends BaseScalaModule {

  override def ivyDeps = Agg(
    ivy"dev.zio::zio:1.0.4",
    ivy"dev.zio::zio-actors:0.0.9",
    ivy"dev.zio::zio-nio:1.0.0-RC10",
    ivy"net.lingala.zip4j:zip4j:2.6.4"
  )

  override def moduleDeps = Seq(`siq-parser`, model)

  object test extends Tests with MillTests
}

object client extends BaseScalaModule {

  override def ivyDeps = Agg(
    ivy"dev.zio::zio:1.0.4",
    ivy"dev.zio::zio-actors:0.0.9",
    ivy"dev.zio::zio-interop-cats:2.2.0.1",
    ivy"org.fomkin::korolev-akka:0.17-M3",
    ivy"org.fomkin::korolev-zio:0.17-M3",
    ivy"org.fomkin::korolev-http4s:0.17-M3",
    ivy"org.http4s::http4s-server:0.21.18",
    ivy"org.http4s::http4s-dsl:0.21.18",
    ivy"org.http4s::http4s-blaze-server:0.21.18"
  )

  override def moduleDeps = Seq(core, `siq-parser`)

  object test extends Tests with MillTests

}
