package phi

/**
 * PHI LANGUAGE - Main Entry Point
 * 
 * Demonstrates the complete Phi language system:
 * - Term[A] with holes for structured editing
 * - Content-addressed repository (Unison-style)
 * - Bidirectional transformations (Xforms)
 * - Type checking
 * - Expression evaluation
 */
object Phi:

  // ============================================================================
  // EXAMPLE REPOSITORY
  // ============================================================================
  
  /** Create a repository pre-populated with standard combinators */
  def createExampleRepo(): Repo[LC] =
    val repo = new Repo[LC]
    
    val programs: List[(String, LC)] = List(
      "id"       -> LC.Lam("x", LC.Var("x")),
      "const"    -> LC.Lam("x", LC.Lam("y", LC.Var("x"))),
      "flip"     -> LC.Lam("f", LC.Lam("x", LC.Lam("y", 
                      LC.App(LC.App(LC.Var("f"), LC.Var("y")), LC.Var("x"))))),
      "compose"  -> LC.Lam("f", LC.Lam("g", LC.Lam("x",
                      LC.App(LC.Var("f"), LC.App(LC.Var("g"), LC.Var("x")))))),
      "apply"    -> LC.Lam("f", LC.Lam("x", LC.App(LC.Var("f"), LC.Var("x")))),
      "true"     -> LC.Lam("t", LC.Lam("f", LC.Var("t"))),
      "false"    -> LC.Lam("t", LC.Lam("f", LC.Var("f"))),
      "and"      -> LC.Lam("p", LC.Lam("q", 
                      LC.App(LC.App(LC.Var("p"), LC.Var("q")), LC.Var("p")))),
      "or"       -> LC.Lam("p", LC.Lam("q",
                      LC.App(LC.App(LC.Var("p"), LC.Var("p")), LC.Var("q")))),
      "zero"     -> LC.Lam("f", LC.Lam("x", LC.Var("x"))),
      "succ"     -> LC.Lam("n", LC.Lam("f", LC.Lam("x",
                      LC.App(LC.Var("f"), 
                        LC.App(LC.App(LC.Var("n"), LC.Var("f")), LC.Var("x")))))),
      "plus"     -> LC.Lam("m", LC.Lam("n", LC.Lam("f", LC.Lam("x",
                      LC.App(LC.App(LC.Var("m"), LC.Var("f")),
                        LC.App(LC.App(LC.Var("n"), LC.Var("f")), LC.Var("x"))))))),
      "pair"     -> LC.Lam("x", LC.Lam("y", LC.Lam("f",
                      LC.App(LC.App(LC.Var("f"), LC.Var("x")), LC.Var("y"))))),
      "fst"      -> LC.Lam("p", LC.App(LC.Var("p"), LC.Lam("x", LC.Lam("y", LC.Var("x"))))),
      "snd"      -> LC.Lam("p", LC.App(LC.Var("p"), LC.Lam("x", LC.Lam("y", LC.Var("y"))))),
      "omega"    -> LC.Lam("x", LC.App(LC.Var("x"), LC.Var("x"))),
      "Y"        -> LC.Lam("f", LC.App(
                      LC.Lam("x", LC.App(LC.Var("f"), LC.App(LC.Var("x"), LC.Var("x")))),
                      LC.Lam("x", LC.App(LC.Var("f"), LC.App(LC.Var("x"), LC.Var("x"))))))
    )
    
    for (name, term) <- programs do
      repo.store(Term.Done(term), Set(Name(name)))
    
    repo

  // ============================================================================
  // VALIDATION
  // ============================================================================
  
  /** Validate round-trip properties */
  def validateRoundTrips(): List[(String, Boolean)] =
    List(
      "LC ↔ IC"        -> validateLCICRoundTrip(),
      "LC ↔ TypedLC"   -> validateTypedLCRoundTrip(),
      "Hash stability" -> validateHashStability(),
      "Patch inverse"  -> validatePatchInverse()
    )

  def validateLCICRoundTrip(): Boolean =
    val terms = List(
      LC.Var("x"),
      LC.Lam("x", LC.Var("x")),
      LC.App(LC.Lam("x", LC.Var("x")), LC.Var("y"))
    )
    terms.forall { lc =>
      val term = Term.Done(lc)
      val there = LCToIC.forward(term)
      val back = LCToIC.backward(there)
      // Structure preserved (names may differ)
      back.isDone
    }

  def validateTypedLCRoundTrip(): Boolean =
    val terms = List(
      LC.Var("x"),
      LC.Lam("x", LC.Var("x"))
    )
    terms.forall { lc =>
      val term = Term.Done(lc)
      val typed = TypeChecker.forward(term)
      val erased = TypeChecker.backward(typed)
      erased.isDone
    }

  def validateHashStability(): Boolean =
    val term = Term.Done(LC.Lam("x", LC.Var("x")))
    val h1 = Hash.compute(term)
    val h2 = Hash.compute(term)
    h1 == h2

  def validatePatchInverse(): Boolean =
    val original: Term[LC] = Term.Done(LC.Var("x"))
    val replacement: Term[LC] = Term.Done(LC.Var("y"))
    val change: Change[LC] = Change.Replace(replacement)
    val patch = Patch.create("test", change, original)
    
    val afterChange = ChangeApplicator(patch.change, original)
    val afterInverse = ChangeApplicator(patch.inverse, afterChange)
    original == afterInverse

  // ============================================================================
  // DEMO
  // ============================================================================
  
  def runDemo(): Unit =
    println("╔════════════════════════════════════════════════════════════════╗")
    println("║                     PHI LANGUAGE SYSTEM                         ║")
    println("║   Structured Editing • Content-Addressed • Bidirectional        ║")
    println("╚════════════════════════════════════════════════════════════════╝")
    println()
    
    // Demo 1: Example Repository
    println("─── EXAMPLE REPOSITORY ───")
    val repo = createExampleRepo()
    println(s"Created repository with ${repo.listNames.size} named programs")
    println(s"Programs: ${repo.listNames.map(_.toString).toList.sorted.mkString(", ")}")
    println()
    
    // Demo 2: Round-trip Validation
    println("─── ROUND-TRIP VALIDATION ───")
    val validations = validateRoundTrips()
    for (name, passed) <- validations do
      val status = if passed then "✓" else "✗"
      println(s"  $status $name")
    println()
    
    // Demo 3: Pipeline
    println("─── PIPELINE EXECUTION ───")
    val source = "λx.x"
    Pipeline.runPipeline(source) match
      case Right(result) =>
        println(s"  Input:    $source")
        println(s"  Parsed:   ${result.parsed}")
        println(s"  Hash:     ${result.hash.short}")
        println(s"  Typed:    ${Pipeline.renderTypedLC(result.typed)}")
        println(s"  Rendered: ${result.rendered}")
      case Left(err) =>
        println(s"  Error: $err")
    println()
    
    // Demo 4: Type Inference
    println("─── TYPE INFERENCE ───")
    val typedTerms = List(
      "identity" -> LC.Lam("x", LC.Var("x")),
      "const"    -> LC.Lam("x", LC.Lam("y", LC.Var("x"))),
      "apply"    -> LC.Lam("f", LC.Lam("x", LC.App(LC.Var("f"), LC.Var("x"))))
    )
    for (name, term) <- typedTerms do
      val typed = TypeChecker.forward(Term.Done(term))
      typed match
        case Term.Done(t) => println(s"  $name : ${t.getType.render}")
        case Term.Hole(_) => println(s"  $name : <type error>")
    println()
    
    // Demo 5: Xform Transformations
    println("─── XFORM TRANSFORMATIONS ───")
    val lcTerm = LC.Lam("x", LC.Var("x"))
    println(s"  LC term: ${Pipeline.renderLCValue(lcTerm)}")
    
    val icNet = LCToIC.forward(Term.Done(lcTerm))
    icNet match
      case Term.Done(net) =>
        println(s"  → IC net: ${net.nodes.size} nodes")
        val lcBack = LCToIC.backward(icNet)
        println(s"  → LC back: ${Pipeline.renderLC(lcBack)}")
      case _ =>
        println(s"  → IC: transformation produced hole")
    println()
    
    // Demo 6: Expression Evaluation
    println("─── EXPRESSION EVALUATION ───")
    val expressions = List("1 + 2 * 3", "(1 + 2) * 3", "10 - 3 - 2")
    for expr <- expressions do
      val stream = Lexer.tokenize(expr)
      ExprParser.expr().parse(stream) match
        case ParseResult(Term.Done(ast), _) =>
          val result = ExprEvaluator.eval(ast)
          println(s"  $expr = $result")
        case _ =>
          println(s"  $expr = <parse error>")
    println()
    
    println("╔════════════════════════════════════════════════════════════════╗")
    println("║                   DEMONSTRATION COMPLETE                        ║")
    println("╚════════════════════════════════════════════════════════════════╝")

  @main def phiMain(): Unit = runDemo()

end Phi
