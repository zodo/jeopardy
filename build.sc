import mill._, scalalib._
import mill.modules.Jvm
import ammonite.ops._

object model extends ScalaModule {
  def scalaVersion = "2.13.4"
}

object `siq-parser` extends ScalaModule {
  def scalaVersion = "2.13.4"

  def scalaxbIvyDeps = T {
    Agg(ivy"org.scalaxb::scalaxb:1.8.0")
  }

  object test extends Tests {
    override def ivyDeps = Agg(ivy"com.lihaoyi::utest:0.7.7")
    def testFrameworks = Seq("utest.runner.Framework")
  }

  override def ivyDeps =
    scalaxbIvyDeps() ++ Agg(
      ivy"org.glassfish.jaxb:jaxb-runtime:2.3.2"
    )

  override def generatedSources = T[Seq[PathRef]] {
    val dest = T.ctx().dest
    val source = PathRef(millSourcePath / "resources" / "ygpackage.xsd")
    val args: Seq[String] = Seq(
      "--outdir",
      dest.toString,
      "--default-package",
      "com.jeopardy.siq.xml",
      "--package-dir",
      "--ignore-unknown",
      "--no-dispatch-client",
      source.path.toString()
    )
    T.ctx().log.info(s"scalaxb ${args.mkString(" ")}")

    Jvm.runSubprocess(
      "scalaxb.compiler.Main",
      resolveDeps(scalaxbIvyDeps)().map(_.path),
      forkArgs(),
      forkEnv(),
      args,
      workingDir = forkWorkingDir()
    )

    Seq(PathRef(dest)).flatMap(p => ls.rec(p.path)).map(PathRef(_))
  }

  override def moduleDeps = Seq(model)
}
