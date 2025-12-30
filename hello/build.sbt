// ═══════════════════════════════════════════════════════════════════════════
// Φ-Hello: Algebraic Metaprogramming Framework
// ═══════════════════════════════════════════════════════════════════════════
//
// A minimal but complete demonstration of:
//   - Algebraic data types and pattern functors
//   - Recursion schemes (cata, ana, hylo, para)
//   - Free/Cofree structures for effects and annotations
//   - Bidirectional parsing and syntax
//   - .phi language specifications
//
// ═══════════════════════════════════════════════════════════════════════════

val scala3Version = "3.7.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "phi-hello",
    version := "0.1.0",
    
    scalaVersion := scala3Version,
    
    // Compiler options for best Scala 3 experience
    scalacOptions ++= Seq(
      "-deprecation",           // Warn about deprecations
      "-feature",               // Warn about advanced features
      "-unchecked",             // Enable additional warnings
    ),
    
    // Dependencies
    libraryDependencies ++= Seq(
      // Parser combinators for .phi file parsing
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      
      // Testing
      "org.scalameta" %% "munit" % "1.0.0" % Test,
    ),
    
    // Test framework
    testFrameworks += new TestFramework("munit.Framework"),
    
    // Main class for running
    Compile / mainClass := Some("phi.Run"),
  )
