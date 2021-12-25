ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.ghidei"
ThisBuild / organizationName := "ghidei"

lazy val core = (project in file("./modules/core/."))
  .settings(
    name := "core",
    libraryDependencies ++= (Dependencies.cats ++ Dependencies.mUnit)
  )

lazy val circe = (project in file("./modules/circe/."))
  .settings(
    name := "circe",
    libraryDependencies ++= (Dependencies.circe ++ Dependencies.mUnit)
  )
  .dependsOn(core)
