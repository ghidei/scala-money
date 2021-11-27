import sbt._

object Dependencies {
  val circeVersion = "0.14.1"

  val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)

  val munit = "org.scalameta" %% "munit" % "0.7.29"

  val deps = circe :+ munit % Test
}
