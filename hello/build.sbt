val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "phi-hello",
    version := "0.1.0",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq("-deprecation", "-feature"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
    )
  )
