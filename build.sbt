import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.minhash",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "tutorial",
    libraryDependencies += scalaTest % Test
  )
