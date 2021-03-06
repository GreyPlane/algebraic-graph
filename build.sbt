ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"
//
//lazy val plugin = project.in(file("plugin"))
//  .settings(crossVersionSharedSources)
//  .settings(publishSettings)
//  .settings(
//    name := "shapeless-plugin",
//    moduleName := "shapeless-plugin",
//    sbtPlugin := true,
//    scalaVersion := Scala213,
//    crossScalaVersions := Seq(Scala213, Scala212)
//  )

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
  "org.typelevel" %% "cats-core" % "2.7.0",
  "org.typelevel" %% "kittens" % "3.0.0-M1",
  "org.scalatest" %% "scalatest" % "3.2.11" % Test
)