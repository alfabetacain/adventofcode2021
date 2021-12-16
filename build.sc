import mill._, scalalib._

object solution extends ScalaModule {
  def scalaVersion      = "2.13.7"
  val catsVersion       = "2.6.1"
  val catsEffectVersion = "3.2.9"
  val fs2Version        = "3.2.2"
  val monocleVersion    = "3.1.0"

  def scalacOptions = Seq("-Ymacro-annotations")
  def forkArgs      = Seq("-Xmx4g")

  def ivyDeps = Agg(
    ivy"co.fs2::fs2-core:${fs2Version}",
    ivy"co.fs2::fs2-io:${fs2Version}",
    ivy"org.typelevel::cats-core:${catsVersion}",
    ivy"org.typelevel::cats-effect:${catsEffectVersion}",
    ivy"dev.optics::monocle-core:${monocleVersion}",
    ivy"dev.optics::monocle-macro:${monocleVersion}",
    ivy"com.lihaoyi::fastparse:2.3.3",
    ivy"org.scodec::scodec-core:1.11.9"
  )

  def scalacPluginIvyDeps = Agg(
    ivy"com.olegpy::better-monadic-for:0.3.1"
  )
}
