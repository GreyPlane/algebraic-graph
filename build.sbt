ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

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
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "kittens" % "3.4.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)