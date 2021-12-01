import mill._, scalalib._

object solution extends ScalaModule {
  def scalaVersion      = "2.13.7"
  val catsVersion       = "2.6.1"
  val catsEffectVersion = "3.2.9"
  val fs2Version        = "3.2.2"

  def ivyDeps = Agg(
    ivy"co.fs2::fs2-core:${fs2Version}",
    ivy"co.fs2::fs2-io:${fs2Version}",
    ivy"org.typelevel::cats-core:${catsVersion}",
    ivy"org.typelevel::cats-effect:${catsEffectVersion}"
  )
}
