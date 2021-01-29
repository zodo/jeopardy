import mill._, scalalib._

object model extends ScalaModule {
  def scalaVersion = "2.13.4"
}

object `siq-parser` extends ScalaModule {
  def scalaVersion = "2.13.4"

  override def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-xml:1.3.0"
  )

  override def moduleDeps = Seq(model)

  object test extends Tests {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.7")
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
