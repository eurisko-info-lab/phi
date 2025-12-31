package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║              Φ-GRAMMAR: Grammar Interpreter (The Brain)                   ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  Execute grammar rules to parse strings into ASTs and render back        ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * This is the missing piece that makes `parse term "λA:*. λx:A. x"` work!
 *
 * BIDIRECTIONAL GRAMMARS
 * ======================
 * 
 * The grammar rules work both ways:
 * 
 *   grammar term {
 *     "λ" IDENT ":" term "." term => Lam(IDENT, term, term)
 *   }
 * 
 * PARSE:  "λx:*. x" → Lam("x", Star, Var("x"))
 * RENDER: Lam("x", Star, Var("x")) → "λx:*. x"
 *
 * HOW IT WORKS
 * ============
 * 
 * The interpreter converts GrammarDecl into parser combinators dynamically,
 * then uses those to parse input strings into Val (AST nodes).
 */
object Grammar:
  import Core.*
  import Core.Val.*
  import Lang.*
  import scala.util.parsing.combinator.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: Grammar Interpreter (Parsing)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * GrammarInterp: Interpret grammar rules to parse strings.
   * 
   * Given a LangSpec with grammar declarations, this creates a parser
   * that can parse strings according to those grammars.
   */
  class GrammarInterp(spec: LangSpec) extends RegexParsers:
    // Skip whitespace between tokens
    override val whiteSpace = """\s*""".r
    
    // Cache of parsers for each sort (to handle recursion)
    private var parserCache: Map[String, Parser[Val]] = Map.empty
    
    // ─────────────────────────────────────────────────────────────────────────
    // Built-in Terminals
    // ─────────────────────────────────────────────────────────────────────────
    
    /** Identifier: starts with letter, contains alphanumeric */
    def IDENT: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
    
    /** Integer literal */
    def INT: Parser[Int] = """-?[0-9]+""".r ^^ (_.toInt)
    
    /** String literal (double-quoted) */
    def STRING: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""
    
    // ─────────────────────────────────────────────────────────────────────────
    // Core Parser Generation
    // ─────────────────────────────────────────────────────────────────────────
    
    /**
     * Get or create a parser for a sort.
     * 
     * This handles recursive grammars by caching parsers.
     */
    def parserFor(sort: String): Parser[Val] =
      parserCache.getOrElse(sort, {
        // Create a lazy parser to handle recursion
        lazy val p: Parser[Val] = spec.grammarFor(sort) match
          case Some(grammar) => 
            // Convert all productions to parsers and combine with |
            grammar.productions.map(productionParser).reduceLeft(_ | _)
          case None =>
            // No grammar defined - try to match as a bare identifier
            failure(s"No grammar defined for sort: $sort")
        
        parserCache = parserCache + (sort -> p)
        p
      })
    
    /**
     * Convert a single production into a parser.
     * 
     * Production like:
     *   "λ" IDENT ":" term "." term => Lam(IDENT, term, term)
     * 
     * Becomes a parser that:
     *   1. Parses the pattern (LHS)
     *   2. Extracts captured values
     *   3. Constructs the AST (RHS)
     */
    def productionParser(prod: Production): Parser[Val] =
      patternParser(prod.pattern).map { captures =>
        // Build the constructor with captured values
        VCon(prod.constructor, captures)
      }
    
    /**
     * Convert a grammar pattern into a parser that returns captured values.
     */
    def patternParser(pat: GrammarPat): Parser[List[Val]] = pat match
      case GrammarPat.Literal(text) =>
        // Match literal text, capture nothing
        literal(text) ^^^ Nil
      
      case GrammarPat.Regex(pattern) =>
        // Match regex, capture as string
        regex(pattern.r) ^^ (s => List(VStr(s)))
      
      case GrammarPat.NonTerminal(sort) =>
        // Special handling for built-in terminals
        sort match
          case "IDENT" => IDENT ^^ (s => List(VStr(s)))
          case "INT"   => INT ^^ (i => List(VInt(i)))
          case "STRING" => STRING ^^ (s => List(VStr(s)))
          case _ =>
            // Parse the sort recursively, capture the result
            parserFor(sort) ^^ (v => List(v))
      
      case GrammarPat.Seq(parts) =>
        // Sequence: parse each part, concatenate captures
        parts.foldLeft(success(Nil): Parser[List[Val]]) { (acc, p) =>
          (acc ~ patternParser(p)) ^^ { case vs1 ~ vs2 => vs1 ++ vs2 }
        }
      
      case GrammarPat.Alt(options) =>
        // Alternative: try each option
        options.map(patternParser).reduceLeft(_ | _)
      
      case GrammarPat.Opt(inner) =>
        // Optional: 0 or 1
        opt(patternParser(inner)) ^^ {
          case Some(vs) => vs
          case None => Nil
        }
      
      case GrammarPat.Rep(inner) =>
        // Repetition: 0 or more
        rep(patternParser(inner)) ^^ (_.flatten)
      
      case GrammarPat.Rep1(inner) =>
        // Repetition: 1 or more
        rep1(patternParser(inner)) ^^ (_.flatten)
      
      case GrammarPat.Group(inner) =>
        // Grouping: just parse inner
        patternParser(inner)
    
    /**
     * Parse a string as a particular sort.
     */
    def parse(sort: String, input: String): Either[String, Val] =
      parseAll(parserFor(sort), input) match
        case Success(result, _) => Right(result)
        case Failure(msg, next) => 
          Left(s"Parse error at ${next.pos}: $msg")
        case Error(msg, next) =>
          Left(s"Fatal parse error at ${next.pos}: $msg")

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: Renderer (AST → String)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * GrammarRenderer: Convert AST back to string using grammar rules.
   * 
   * This is the reverse of parsing - given a Val, produce the string
   * representation according to the grammar.
   */
  class GrammarRenderer(spec: LangSpec):
    
    /**
     * Render a value using the grammar for its sort.
     */
    def render(v: Val): String = v match
      case VCon(name, args) =>
        // Find a production that matches this constructor
        findProduction(name) match
          case Some(prod) => renderProduction(prod, args)
          case None => 
            // No production - use default rendering
            if args.isEmpty then name
            else s"$name(${args.map(render).mkString(", ")})"
      
      case VStr(s) => s
      case VInt(i) => i.toString
      case VList(elems) => s"[${elems.map(render).mkString(", ")}]"
    
    /**
     * Find the production for a constructor.
     */
    private def findProduction(constructor: String): Option[Production] =
      spec.grammars.flatMap(_.productions).find(_.constructor == constructor)
    
    /**
     * Render a value using a specific production.
     */
    private def renderProduction(prod: Production, args: List[Val]): String =
      var remaining = args
      renderPattern(prod.pattern, () => {
        val v = remaining.head
        remaining = remaining.tail
        v
      })
    
    /**
     * Render a pattern, consuming values as needed.
     */
    private def renderPattern(pat: GrammarPat, next: () => Val): String = pat match
      case GrammarPat.Literal(text) => text
      
      case GrammarPat.Regex(_) => render(next())
      
      case GrammarPat.NonTerminal(sort) =>
        sort match
          case "IDENT" | "INT" | "STRING" => render(next())
          case _ => render(next())
      
      case GrammarPat.Seq(parts) =>
        parts.map(p => renderPattern(p, next)).mkString(" ")
      
      case GrammarPat.Alt(options) =>
        // For rendering, just use first option (simplification)
        renderPattern(options.head, next)
      
      case GrammarPat.Opt(inner) =>
        // For rendering, render if value present
        renderPattern(inner, next)
      
      case GrammarPat.Rep(inner) =>
        renderPattern(inner, next)
      
      case GrammarPat.Rep1(inner) =>
        renderPattern(inner, next)
      
      case GrammarPat.Group(inner) =>
        renderPattern(inner, next)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: XformInterp (Execute Transformations)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * XformInterp: Execute xform rules on values.
   */
  class XformInterp(spec: LangSpec):
    import Meta.*
    
    /**
     * Apply an xform to a value.
     */
    def apply(xformName: String, input: Val): Either[String, Val] =
      spec.xformNamed(xformName) match
        case Some(xform) => applyXform(xform, input)
        case None => Left(s"Unknown xform: $xformName")
    
    /**
     * Apply an xform, trying each case in order.
     */
    private def applyXform(xform: XformDecl, input: Val): Either[String, Val] =
      xform.cases.view
        .flatMap { case XformCase(pat, body) =>
          pat.matchAgainst(input).map { env =>
            body.eval(env)
          }
        }
        .headOption match
          case Some(result) => Right(result)
          case None => Left(s"No matching case in ${xform.name} for: ${input.show}")
    
    /**
     * Apply xform recursively (bottom-up).
     */
    def applyRecursive(xformName: String, input: Val): Either[String, Val] =
      // First transform children
      val transformedChildren = input match
        case VCon(name, args) =>
          val newArgs = args.map(arg => applyRecursive(xformName, arg))
          val errs = newArgs.collect { case Left(e) => e }
          if errs.nonEmpty then return Left(errs.mkString("; "))
          VCon(name, newArgs.collect { case Right(v) => v })
        case other => other
      
      // Then try to transform this node
      apply(xformName, transformedChildren) match
        case Right(result) => Right(result)
        case Left(_) => Right(transformedChildren) // No rule matched, keep as-is

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 4: Strategy Interpreter
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * StrategyInterp: Execute reduction strategies.
   */
  class StrategyInterp(spec: LangSpec):
    private val xformInterp = XformInterp(spec)
    
    /**
     * Normalize a term by repeatedly applying a strategy.
     */
    def normalize(strategyName: String, input: Val, maxSteps: Int = 1000): Either[String, Val] =
      var current = input
      var steps = 0
      
      while steps < maxSteps do
        xformInterp.apply(strategyName, current) match
          case Right(next) if next != current =>
            current = next
            steps += 1
          case _ =>
            return Right(current) // Fixed point reached
      
      Left(s"Normalization did not converge after $maxSteps steps")
    
    /**
     * Apply a strategy once.
     */
    def step(strategyName: String, input: Val): Either[String, Val] =
      xformInterp.apply(strategyName, input)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 5: LangRunner (Main Entry Point)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * LangRunner: Execute a complete language specification.
   * 
   * This is the main entry point for running .phi specifications.
   */
  class LangRunner(spec: LangSpec):
    val parser = GrammarInterp(spec)
    val renderer = GrammarRenderer(spec)
    val xforms = XformInterp(spec)
    val strategies = StrategyInterp(spec)
    
    /**
     * Parse a string as a sort.
     */
    def parse(sort: String, input: String): Either[String, Val] =
      parser.parse(sort, input)
    
    /**
     * Render a value back to string.
     */
    def render(v: Val): String =
      renderer.render(v)
    
    /**
     * Apply a transformation.
     */
    def transform(xformName: String, input: Val): Either[String, Val] =
      xforms.apply(xformName, input)
    
    /**
     * Normalize using a strategy.
     */
    def normalize(strategyName: String, input: Val): Either[String, Val] =
      strategies.normalize(strategyName, input)
    
    /**
     * Full pipeline: parse → transform → render
     */
    def pipeline(
      sort: String,
      input: String,
      xformName: Option[String] = None,
      strategyName: Option[String] = None
    ): Either[String, String] =
      for
        parsed <- parse(sort, input)
        transformed <- xformName.map(xforms.apply(_, parsed)).getOrElse(Right(parsed))
        normalized <- strategyName.map(strategies.normalize(_, transformed)).getOrElse(Right(transformed))
      yield render(normalized)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 6: Convenience Functions
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Quick parse from a spec.
   */
  def parse(spec: LangSpec, sort: String, input: String): Either[String, Val] =
    GrammarInterp(spec).parse(sort, input)
  
  /**
   * Quick render from a spec.
   */
  def render(spec: LangSpec, v: Val): String =
    GrammarRenderer(spec).render(v)
  
  /**
   * Load a .phi file and create a runner.
   */
  def loadSpec(content: String, name: String = "spec"): Either[String, LangRunner] =
    PhiParser.parseFile(name, content).map(LangRunner(_))
