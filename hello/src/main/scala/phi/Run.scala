package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║                     Φ-RUN: Interpreter & Examples                         ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  Run .phi specifications, demonstrate Core features, load examples        ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * This module provides:
 *   1. Runner for interpreting .phi language specifications
 *   2. Examples demonstrating Core's algebraic features
 *   3. Interactive REPL for exploration
 *   4. Test harness for validation
 */
object Run:
  import Core.*
  import Core.Val.*
  import Meta.*
  import Lang.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: Examples Using Core
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Example ASTs for demonstration.
   */
  object Examples:
    // Simple arithmetic expression: (1 + 2) * 3
    val expr1: Val = VCon("Mul", List(
      VCon("Add", List(VCon("Lit", List(VInt(1))), VCon("Lit", List(VInt(2))))),
      VCon("Lit", List(VInt(3)))
    ))
    
    // Lambda calculus: (λx. x + 1) 5
    val lambda1: Val = VCon("App", List(
      VCon("Lam", List(VStr("x"), VCon("Add", List(VCon("Var", List(VStr("x"))), VCon("Lit", List(VInt(1))))))),
      VCon("Lit", List(VInt(5)))
    ))
    
    // List of numbers
    val numList: Val = VList(List(VInt(1), VInt(2), VInt(3), VInt(4), VInt(5)))
    
    // Binary tree
    val tree: Val = VCon("Node", List(
      VInt(1),
      VCon("Node", List(VInt(2), VCon("Leaf", Nil), VCon("Leaf", Nil))),
      VCon("Node", List(VInt(3), VCon("Leaf", Nil), VCon("Leaf", Nil)))
    ))

  /**
   * Recursion Scheme Examples.
   * 
   * These demonstrate cata, ana, hylo, and para on real ASTs.
   */
  object RecursionExamples:
    
    /**
     * Example 1: Count nodes using catamorphism.
     * 
     * cata processes bottom-up, so we get counts from children first.
     */
    def countNodes(v: Val): Int = cata[Int] {
      case C(_, args) => 1 + args.sum
      case L(elems)   => 1 + elems.sum
      case _          => 1
    }(v)
    
    /**
     * Example 2: Evaluate arithmetic expressions.
     * 
     * Demonstrates pattern matching in the algebra.
     */
    def eval(v: Val): Int = cata[Int] {
      case C("Lit", List(n))    => n
      case C("Add", List(l, r)) => l + r
      case C("Mul", List(l, r)) => l * r
      case C("Sub", List(l, r)) => l - r
      case I(n)                 => n
      case _                    => 0
    }(v)
    
    /**
     * Example 3: Generate a Peano number using anamorphism.
     * 
     * ana unfolds a seed into a tree structure.
     */
    def peano(n: Int): Val = ana[Int] { n =>
      if n <= 0 then C("Zero", Nil)
      else C("Succ", List(n - 1))
    }(n)
    
    /**
     * Example 4: Fibonacci via hylomorphism.
     * 
     * Unfold into call tree, then fold to compute result.
     * The intermediate tree is never fully materialized!
     */
    def fib(n: Int): Int = hylo[Int, Int](
      // Algebra: compute from children
      {
        case C("Base", Nil)       => 1
        case C("Rec", List(a, b)) => a + b
        case I(n)                 => n
        case _                    => 0
      },
      // Coalgebra: generate call structure
      n => if n <= 1 then C("Base", Nil) else C("Rec", List(n - 1, n - 2))
    )(n)
    
    /**
     * Example 5: Show expression with original structure (paramorphism).
     * 
     * para gives access to both the computed result AND the original subtree.
     */
    def showWithDepth(v: Val): String = para[String] {
      case C("Add", List((_, l), (_, r))) => s"($l + $r)"
      case C("Mul", List((_, l), (_, r))) => s"($l × $r)"
      case C("Lit", List((VInt(n), _)))   => n.toString
      case I(n)                           => n.toString
      case C(name, args)                  => s"$name(${args.map(_._2).mkString(", ")})"
      case _                              => "?"
    }(v)

  /**
   * Zipper Examples.
   * 
   * Navigation and modification with context.
   */
  object ZipperExamples:
    /**
     * Navigate to a specific position and modify.
     */
    def modifyAt(v: Val, path: List[Int], f: Val => Val): Val =
      Zipper.navigate(Zipper.from(v), path) match
        case Some(z) => Zipper.toVal(Zipper.modify(z)(f))
        case None    => v
    
    /**
     * Collect all paths to leaves.
     */
    def leafPaths(v: Val): List[List[Int]] =
      val z = Zipper.from(v)
      def collect(z: Zipper): List[List[Int]] = z.tail match
        case C(_, Nil) => List(z.head.path)
        case L(Nil)    => List(z.head.path)
        case S(_)      => List(z.head.path)
        case I(_)      => List(z.head.path)
        case C(_, args) => args.flatMap(collect)
        case L(elems)   => elems.flatMap(collect)
      collect(z)

  /**
   * Validation Examples.
   * 
   * Parallel error accumulation with Validated.
   */
  object ValidationExamples:
    
    /** Validation that a name is non-empty */
    def validateName(s: String): Validated[String, String] =
      if s.nonEmpty then Valid(s)
      else Invalid(List("Name cannot be empty"))
    
    /** Validation that age is positive */
    def validateAge(n: Int): Validated[String, Int] =
      if n > 0 then Valid(n)
      else Invalid(List("Age must be positive"))
    
    /** Validation that email contains @ */
    def validateEmail(s: String): Validated[String, String] =
      if s.contains("@") then Valid(s)
      else Invalid(List("Email must contain @"))
    
    /**
     * Validate a person record - collects ALL errors.
     */
    def validatePerson(name: String, age: Int, email: String): Validated[String, (String, Int, String)] =
      (validateName(name) zip validateAge(age) zip validateEmail(email)).map {
        case ((n, a), e) => (n, a, e)
      }

  /**
   * Optics Examples.
   * 
   * Focusing and transforming nested structures.
   */
  object OpticsExamples:
    
    /** Focus on the left child of a binary node */
    val leftChild: Lens[Val, Val] = Optics.arg(0)
    
    /** Focus on the right child of a binary node */
    val rightChild: Lens[Val, Val] = Optics.arg(1)
    
    /** Double all integer literals in an expression */
    def doubleInts(v: Val): Val = Optics.everywhere {
      case VCon("Lit", List(VInt(n))) => VCon("Lit", List(VInt(n * 2)))
      case VInt(n) => VInt(n * 2)
      case other => other
    }(v)
    
    /** Replace all variables with a constant */
    def constify(v: Val, c: Int): Val = Optics.everywhere {
      case VCon("Var", _) => VCon("Lit", List(VInt(c)))
      case other => other
    }(v)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: .phi File Runner
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Load and run a .phi specification.
   */
  def loadSpec(name: String, content: String): Either[String, LangSpec] =
    PhiParser.parseFile(name, content)
  
  /**
   * Load a .phi file from disk.
   */
  def loadFile(path: String): Either[String, LangSpec] =
    try
      val content = scala.io.Source.fromFile(path).mkString
      val name = path.split("/").last.stripSuffix(".phi")
      loadSpec(name, content)
    catch
      case e: Exception => Left(s"Failed to load $path: ${e.getMessage}")
  
  /**
   * Run a transform from a loaded spec.
   */
  def runXform(spec: LangSpec, xformName: String, input: Val): Either[String, Val] =
    applyXform(spec, xformName, input).toRight(s"Transform '$xformName' did not match")

  /**
   * Demonstrate the grammar pipeline with real .phi specs.
   */
  def runGrammarDemo(): Unit =
    // Define a simple arithmetic language using the actual PhiParser syntax
    val arithSpec = """
      |sort Expr
      |
      |Expr = Lit(value: Int)
      |     | Add(left: Expr, right: Expr)
      |     | Mul(left: Expr, right: Expr)
      |     | Var(name: String)
      |
      |grammar expr {
      |  Lit    <- INT
      |  Var    <- IDENT
      |  Add    <- expr "+" expr
      |  Mul    <- expr "*" expr
      |}
      |""".stripMargin
    
    Grammar.loadSpec(arithSpec, "arith") match
      case Right(runner) =>
        // Parse some expressions
        val inputs = List("42", "x", "1 + 2")
        inputs.foreach { input =>
          runner.parse("expr", input) match
            case Right(ast) =>
              val rendered = runner.render(ast)
              println(s"  parse \"$input\" → ${ast.show}")
              println(s"    render → \"$rendered\"")
            case Left(err) =>
              println(s"  parse \"$input\" → ERROR: $err")
        }
      case Left(err) =>
        println(s"  Failed to load arith spec: $err")
    
    println()
    
    // Try loading CoC and parsing a term
    println("  CoC Lambda Calculus:")
    // Define CoC grammar inline using actual parser syntax
    val cocSpec = """
      |sort Term
      |
      |Term = Star
      |     | Box
      |     | Var(name: String)
      |     | Pi(x: String, t: Term, body: Term)
      |     | Lam(x: String, t: Term, body: Term)
      |     | App(f: Term, a: Term)
      |
      |grammar term {
      |  Star   <- "*"
      |  Box    <- "□"
      |  Var    <- IDENT
      |  Pi     <- "Π" IDENT ":" term "." term
      |  Lam    <- "λ" IDENT ":" term "." term
      |  App    <- "(" term term ")"
      |}
      |""".stripMargin
    
    Grammar.loadSpec(cocSpec, "coc") match
      case Right(runner) =>
        val terms = List("*", "x", "λx:*. x", "(f x)")
        terms.foreach { input =>
          runner.parse("term", input) match
            case Right(ast) => println(s"    parse \"$input\" → ${ast.show}")
            case Left(err) => println(s"    parse \"$input\" → ERROR: $err")
        }
      case Left(err) =>
        println(s"    Failed to load coc spec: $err")

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: Demo Runner
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Run all examples and print results.
   */
  def runDemo(): Unit =
    println("╔═══════════════════════════════════════════════════════════════╗")
    println("║              Φ-HELLO: Algebraic Metaprogramming Demo          ║")
    println("╚═══════════════════════════════════════════════════════════════╝")
    println()
    
    // 1. Recursion schemes
    println("─── Recursion Schemes ───")
    println(s"Expression: ${Examples.expr1.show}")
    println(s"Node count: ${RecursionExamples.countNodes(Examples.expr1)}")
    println(s"Evaluation: ${RecursionExamples.eval(Examples.expr1)}")
    println(s"Peano(3):   ${RecursionExamples.peano(3).show}")
    println(s"Fib(10):    ${RecursionExamples.fib(10)}")
    println(s"Show:       ${RecursionExamples.showWithDepth(Examples.expr1)}")
    println()
    
    // 2. Zipper navigation
    println("─── Zipper Navigation ───")
    val z = Zipper.from(Examples.tree)
    println(s"Tree:       ${Examples.tree.show}")
    println(s"Root:       ${z.head.value.show}")
    Zipper.navigate(z, List(1)) match
      case Some(child) => println(s"At [1]:     ${child.head.value.show}")
      case None => println("Path [1] not found")
    println(s"Leaf paths: ${ZipperExamples.leafPaths(Examples.tree)}")
    println()
    
    // 3. Validation
    println("─── Validation ───")
    println(s"Valid:   ${ValidationExamples.validatePerson("Alice", 30, "alice@example.com")}")
    println(s"Invalid: ${ValidationExamples.validatePerson("", -5, "not-an-email")}")
    println()
    
    // 4. Optics
    println("─── Optics ───")
    println(s"Original: ${Examples.expr1.show}")
    println(s"Doubled:  ${OpticsExamples.doubleInts(Examples.expr1).show}")
    println()
    
    // 5. Hashing
    println("─── Content Addressing ───")
    val h1 = Hash.of(Examples.expr1)
    val h2 = Hash.of(Examples.tree)
    println(s"Hash(expr): ${h1.value}")
    println(s"Hash(tree): ${h2.value}")
    println()
    
    // 6. Grammar Pipeline - Parse using .phi specs!
    println("─── Grammar Pipeline ───")
    runGrammarDemo()
    println()
    
    println("═══════════════════════════════════════════════════════════════")
    println("Demo complete! See src/main/resources/specs/ for core .phi specifications.")

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 4: Main Entry Point
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Main entry point.
   * 
   * Usage:
   *   phi              -- run demo
   *   phi <file.phi>   -- load and show spec
   *   phi <file.phi> <xform> <input>  -- run transform
   */
  def main(args: Array[String]): Unit = args.toList match
    case Nil =>
      runDemo()
    
    case file :: Nil =>
      loadFile(file) match
        case Right(spec) =>
          println(s"Loaded: ${spec.name}")
          println(spec.show)
        case Left(err) =>
          println(s"Error: $err")
    
    case file :: xform :: input :: Nil =>
      loadFile(file) match
        case Right(spec) =>
          // Parse input as a simple expression
          val v = VCon(input, Nil) // Simplified - real impl would parse properly
          runXform(spec, xform, v) match
            case Right(result) => println(s"Result: ${result.show}")
            case Left(err) => println(s"Error: $err")
        case Left(err) =>
          println(s"Error: $err")
    
    case _ =>
      println("Usage: phi [file.phi] [xform] [input]")

// End of Run object
