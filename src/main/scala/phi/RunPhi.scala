package phi

import java.nio.file.{Files, Paths}

/**
 * CLI for running Phi language specifications.
 * 
 * Usage: sbt "runMain phi.RunPhi examples/stlc-nat.phi fib5"
 */
@main def RunPhi(specFile: String, defName: String): Unit =
  println(s"=== Phi Language Runner ===")
  println(s"Loading: $specFile")
  
  // Read file
  val source = try
    Files.readString(Paths.get(specFile))
  catch
    case e: Exception =>
      println(s"Error reading file: ${e.getMessage}")
      return
  
  // Parse
  println("Parsing...")
  val parseResult: Either[String, LangSpec] = PhiParser.parse(source)
  parseResult match
    case Left(err) =>
      println(s"Parse error: $err")
    case Right(spec) =>
      println(s"Language: ${spec.name}")
      println(s"  Sorts: ${spec.sorts.map(_.name).mkString(", ")}")
      println(s"  Constructors: ${spec.constructors.map(_.name).mkString(", ")}")
      println(s"  Rules: ${spec.rules.map(_.name).mkString(", ")}")
      
      // Debug: show first rule
      spec.rules.headOption.foreach { r =>
        println(s"  First rule ${r.name}:")
        r.cases.headOption.foreach { c =>
          println(s"    LHS: ${c.lhs}")
          println(s"    RHS: ${c.rhs}")
        }
      }
      
      println(s"  Definitions: ${spec.defs.map(_.name).mkString(", ")}")
      println(s"  Strategies: ${spec.strategies.keys.mkString(", ")}")
      println()
      
      // Run
      val interp = LangInterpreter(spec)
      
      println(s"Evaluating: $defName")
      try
        val term = interp.evalDef(defName)
        println(s"Initial: ${term.show}")
        println()
        println("Normalizing...")
        
        val start = System.currentTimeMillis()
        val result = interp.normalize(term)
        val elapsed = System.currentTimeMillis() - start
        val rate = if elapsed > 0 then result.steps * 1000 / elapsed else 0
        
        println(s"Result: ${result.value.show}")
        println(s"Time: ${elapsed}ms, Steps: ${result.steps}, Rate: $rate steps/s")
      catch
        case e: Exception =>
          println(s"Error: ${e.getMessage}")
          e.printStackTrace()
