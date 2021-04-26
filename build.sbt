import Dependencies._

ThisBuild / scalaVersion := "2.13.6"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "com.ghidei"
ThisBuild / organizationName := "ghidei"

lazy val root = (project in file("."))
  .settings(
    name := "scala-money",
    libraryDependencies ++= deps
  )
