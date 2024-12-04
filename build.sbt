ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.5.2"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2021",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.7.0"
    )
  )
