import sbt._

object Dependencies {

  lazy val Versions = new {
    val Cats  = "2.7.0"
    val Circe = "0.14.1"
    val MUnit = "0.7.29"
  }

  lazy val circe = Seq(
    "io.circe" %% "circe-generic" % Versions.Circe,
    "io.circe" %% "circe-parser"  % Versions.Circe
  )

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % Versions.Cats
  )

  lazy val mUnit = Seq(
    "org.scalameta" %% "munit" % Versions.MUnit % Test
  )

}
