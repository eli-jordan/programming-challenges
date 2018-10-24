import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "scala-guild-challenges",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "co.fs2" %% "fs2-core" % "0.10.4",
      "co.fs2" %% "fs2-io" % "0.10.4"
    )
  )
