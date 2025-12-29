package phi

/**
 * Simple CLI for evaluating Phi/LC expressions.
 * Usage: sbt "runMain phi.Eval 'let x = 5 in x + 1'"
 */
@main def Eval(input: String): Unit =
  println(s"Input: $input")
  
  Pipeline.parseLC(input) match
    case Left(err) =>
      println(s"Parse error: $err")
    case Right(term) =>
      term match
        case Term.Done(lc) =>
          println(s"Parsed: ${Pipeline.renderLCValue(lc)}")
          try
            val result = LCInterpreter.eval(lc)
            println(s"Result: ${LCInterpreter.render(result)}")
          catch
            case e: Exception =>
              println(s"Eval error: ${e.getMessage}")
        case Term.Hole(_) =>
          println(s"Incomplete term (has holes): ${Pipeline.renderLC(term)}")
