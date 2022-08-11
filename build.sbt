ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

val scalacOptionsAll = List(
  "-feature",
  "-language:higherKinds,implicitConversions",
  "-Xfatal-warnings",
  "-deprecation",
  "-unchecked"
)

lazy val root = (project in file("."))
  .settings(
    name := "algebraic-graph",
    scalacOptions := scalacOptionsAll
  )

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.8.0",
  "org.typelevel" %% "kittens" % "3.0.0-M1",
  "org.scalatest" %% "scalatest" % "3.2.12" % Test
)