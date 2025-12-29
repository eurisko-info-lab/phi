ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.7.4"

val http4sVersion = "0.23.30"
val circeVersion = "0.14.10"

lazy val root = (project in file("."))
  .settings(
    name := "phi",
    libraryDependencies ++= Seq(
      // HTTP server
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      
      // JSON
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      
      // Parser combinators
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      
      // Testing
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.18.1" % Test
    ),
    
    // Enable warnings
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked"
    )
  )
