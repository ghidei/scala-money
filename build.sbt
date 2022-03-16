val baseVersion = "0.0.1"
val Scala213    = "2.13.8"
val Scala3      = "3.1.1"

ThisBuild / tlBaseVersion := baseVersion
ThisBuild / scalaVersion := Scala213
ThisBuild / crossScalaVersions := Seq(Scala213, Scala3)
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / version := baseVersion
ThisBuild / organization := "com.ghidei"
ThisBuild / organizationName := "ghidei"
ThisBuild / developers := List(tlGitHubDev("ghidei", "Yonas Ghidei"))
ThisBuild / tlSonatypeUseLegacyHost := true

lazy val root = tlCrossRootProject.aggregate(core, circe)

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

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0"
inThisBuild(
  List(
    scalaVersion := "2.13.8",
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision
  )
)

addCommandAlias("fmt", "scalafmt")
addCommandAlias("fix", "scalafixAll dependency:OrganizeImports@com.github.liancheng:organize-imports:0.6.0")
addCommandAlias("build", "fmt; fix")
addCommandAlias("check", "all root/scalafmtSbtCheck root/scalafmtCheckAll")
addCommandAlias("testAll", "root/test")
