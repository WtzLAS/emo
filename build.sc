import mill._, scalalib._

object V {
  def zio = "2.0.18"
  def zioLogging = "2.1.14"
  def sttp = "3.9.0"
  def sttpOauth2 = "0.17.0-RC1"
  def lucumaRefined = "0.1.2"
  def circe = "0.14.6"
  def pprint = "0.8.1"
  def chronoscala = "2.0.9"
  def scalaTags = "0.12.0"
  def osLib = "0.9.1"
  def zioCache = "0.2.3"
  def zioConfig = "4.0.0-RC16"
}

object main extends ScalaModule {
  def scalaVersion = "3.3.1"

  def ivyDeps = Agg(
    ivy"dev.zio::zio:${V.zio}",
    ivy"dev.zio::zio-logging:${V.zioLogging}",
    ivy"com.softwaremill.sttp.client3::core:${V.sttp}",
    ivy"com.softwaremill.sttp.client3::zio:${V.sttp}",
    ivy"com.softwaremill.sttp.client3::circe:${V.sttp}",
    ivy"com.ocadotechnology::sttp-oauth2:${V.sttpOauth2}",
    ivy"com.ocadotechnology::sttp-oauth2-circe:${V.sttpOauth2}",
    ivy"edu.gemini::lucuma-refined:${V.lucumaRefined}",
    ivy"io.circe::circe-core:${V.circe}",
    ivy"io.circe::circe-generic:${V.circe}",
    ivy"io.circe::circe-literal:${V.circe}",
    ivy"com.lihaoyi::pprint:${V.pprint}",
    ivy"io.github.chronoscala::chronoscala:${V.chronoscala}",
    ivy"com.lihaoyi::scalatags:${V.scalaTags}",
    ivy"com.lihaoyi::os-lib:${V.osLib}",
    ivy"dev.zio::zio-cache:${V.zioCache}",
    ivy"dev.zio::zio-config:${V.zioConfig}",
    ivy"dev.zio::zio-config-magnolia:${V.zioConfig}",
    ivy"dev.zio::zio-config-typesafe:${V.zioConfig}"
  )
}
