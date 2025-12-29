package phi

import scala.annotation.tailrec

/**
 * INTERTWINING RULES - End-to-end pipeline demonstration
 * 
 * This module demonstrates the complete pipeline:
 *   parsing → Term[A] → repo → editor → changes → Xforms → typecheck → rendering → repo
 */
object Pipeline:

  // ============================================================================
  // STAGE 1: Parsing - Text → Term[LC]
  // ============================================================================
  
  /** Parse LC expression from string */
  def parseLC(source: String): Either[String, Term[LC]] =
    val parser = new LCParser(source.trim)
    parser.parseExpr()

  /** Proper recursive descent parser for LC with operators */
  private class LCParser(input: String):
    private var pos = 0
    
    private def peek: Char = if pos < input.length then input(pos) else '\u0000'
    private def peek2: String = if pos + 1 < input.length then input.substring(pos, pos + 2) else ""
    private def advance(): Char = { val c = peek; pos += 1; c }
    private def atEnd: Boolean = pos >= input.length
    private def skipWs(): Unit = while !atEnd && peek.isWhitespace do advance()
    
    private def parseIdent(): String =
      skipWs()
      val sb = new StringBuilder
      while !atEnd && (peek.isLetterOrDigit || peek == '_') do sb += advance()
      sb.toString
    
    private def parseNumber(): Int =
      skipWs()
      val sb = new StringBuilder
      while !atEnd && peek.isDigit do sb += advance()
      sb.toString.toIntOption.getOrElse(0)
    
    private def expect(s: String): Boolean =
      skipWs()
      if input.substring(pos).startsWith(s) then { pos += s.length; true }
      else false
    
    // Expression = Or
    // Helper to unwrap Term[LC] to LC
    private def unwrap(t: Term[LC]): LC = t.getOrElse(LC.Var("?"))
    
    def parseExpr(): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input")
      else parseWithStop(() => false) // No stop condition for full expressions
    
    // Unified expression parser with configurable stop condition
    private def parseWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseOrWithStop(shouldStop)
    
    // Or = And ('|' And)*
    private def parseOrWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseAndWithStop(shouldStop).flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || shouldStop() then Right(result)
          else if peek == '|' then
            advance()
            parseAndWithStop(shouldStop) match
              case Right(right) => loop(Term.Done(LC.Prim("|", List(unwrap(result), unwrap(right)))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // And = Comparison ('&' Comparison)*
    private def parseAndWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseComparisonWithStop(shouldStop).flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || shouldStop() then Right(result)
          else if peek == '&' then
            advance()
            parseComparisonWithStop(shouldStop) match
              case Right(right) => loop(Term.Done(LC.Prim("&", List(unwrap(result), unwrap(right)))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Comparison = Additive (('=' | '!=' | '<' | '>' | '<=' | '>=') Additive)?
    private def parseComparisonWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseAdditiveWithStop(shouldStop).flatMap { left =>
        skipWs()
        if atEnd || shouldStop() then Right(left)
        else
          val op = 
            if peek2 == "!=" then { pos += 2; Some("!=") }
            else if peek2 == "<=" then { pos += 2; Some("<=") }
            else if peek2 == ">=" then { pos += 2; Some(">=") }
            else if peek == '=' then { advance(); Some("=") }
            else if peek == '<' then { advance(); Some("<") }
            else if peek == '>' then { advance(); Some(">") }
            else None
          op match
            case Some(o) =>
              parseAdditiveWithStop(shouldStop).map { right =>
                Term.Done(LC.Prim(o, List(unwrap(left), unwrap(right))))
              }
            case None => Right(left)
      }
    
    // Additive = Multiplicative (('+' | '-') Multiplicative)*
    private def parseAdditiveWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseMultiplicativeWithStop(shouldStop).flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || shouldStop() then Right(result)
          else if peek == '+' || peek == '-' then
            val op = advance().toString
            parseMultiplicativeWithStop(shouldStop) match
              case Right(right) => loop(Term.Done(LC.Prim(op, List(unwrap(result), unwrap(right)))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Multiplicative = Application (('*' | '/') Application)*
    private def parseMultiplicativeWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseApplicationWithStop(shouldStop).flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || shouldStop() then Right(result)
          else if peek == '*' || peek == '/' then
            val op = advance().toString
            parseApplicationWithStop(shouldStop) match
              case Right(right) => loop(Term.Done(LC.Prim(op, List(unwrap(result), unwrap(right)))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Application = Atom+
    private def parseApplicationWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      parseAtomWithStop(shouldStop).flatMap { first =>
        var result = first
        var continue = true
        while continue do
          skipWs()
          if atEnd || shouldStop() || !isAtomStart(peek) then continue = false
          else
            parseAtomWithStop(shouldStop) match
              case Right(arg) => result = Term.Done(LC.App(unwrap(result), unwrap(arg)))
              case Left(_) => continue = false
        Right(result)
      }
    
    private def isAtomStart(c: Char): Boolean =
      c.isLetter || c.isDigit || c == '(' || c == 'λ' || c == '\\' || c == '?'
    
    private def isKeyword(s: String): Boolean =
      Set("then", "else", "in").contains(s)
    
    private def lookingAtKeyword(): Boolean =
      skipWs()
      if atEnd || !peek.isLetter then false
      else
        val savedPos = pos
        val word = parseIdent()
        pos = savedPos
        isKeyword(word)
    
    // Atom with configurable stop
    private def parseAtomWithStop(shouldStop: () => Boolean): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input")
      else if shouldStop() then Left("Stopped at keyword")
      else peek match
        // Lambda: λx.body or \x.body
        case 'λ' | '\\' =>
          advance()
          skipWs()
          val param = parseIdent()
          skipWs()
          if peek == '.' then advance()
          else if peek2 == "->" || peek2 == "=>" then { advance(); advance() }
          parseWithStop(shouldStop).map(body => Term.Done(LC.Lam(param, unwrap(body))))
        
        // Parenthesized expression - inside parens, no stop condition
        case '(' =>
          advance()
          val result = parseWithStop(() => false)
          skipWs()
          if peek == ')' then advance()
          result
        
        // Hole
        case '?' =>
          advance()
          val label = if !atEnd && peek.isLetter then Some(parseIdent()) else None
          Right(Term.Hole(label))
        
        // Number
        case c if c.isDigit =>
          Right(Term.Done(LC.Lit(parseNumber())))
        
        // Identifier or keyword
        case c if c.isLetter =>
          val savedPos = pos
          val name = parseIdent()
          
          // Check if this is a stop keyword
          if isKeyword(name) then
            pos = savedPos
            Left(s"Stopped at keyword: $name")
          else name match
            case "fn" | "fun" =>
              skipWs()
              val param = parseIdent()
              skipWs()
              expect("=>") || expect("->")
              parseWithStop(shouldStop).map(body => Term.Done(LC.Lam(param, unwrap(body))))
            
            case "let" =>
              skipWs()
              val varName = parseIdent()
              skipWs()
              expect("=")
              // Parse value stopping at 'in' keyword
              val stopAtIn = () => lookingAtKeyword()
              for
                value <- parseWithStop(stopAtIn)
                _ = { skipWs(); expect("in") }
                body <- parseWithStop(shouldStop)
              yield Term.Done(LC.Let(varName, unwrap(value), unwrap(body)))
            
            case "if" =>
              skipWs()
              // Parse condition/branches stopping at then/else keywords
              val stopAtKeyword = () => lookingAtKeyword()
              for
                cond <- parseWithStop(stopAtKeyword)
                _ = { skipWs(); expect("then") }
                thenBr <- parseWithStop(stopAtKeyword)
                _ = { skipWs(); expect("else") }
                elseBr <- parseWithStop(shouldStop)
              yield Term.Done(LC.Prim("if", List(unwrap(cond), unwrap(thenBr), unwrap(elseBr))))
            
            case _ =>
              Right(Term.Done(LC.Var(name)))
        
        case c =>
          Left(s"Unexpected character: $c")

  // ============================================================================
  // STAGE 2: Repository Storage
  // ============================================================================
  
  /** Store a term in the repository */
  def storeInRepo[A](repo: Repo[A], term: Term[A], name: Name): Hash =
    repo.store(term, Set(name))

  // ============================================================================
  // STAGE 3: Editor (Zipper-based)
  // ============================================================================
  
  /** Create an editor for a term */
  def createEditor[A](term: Term[A]): TermZipper[A] =
    TermZipper(term)

  // ============================================================================
  // STAGE 4: Apply Changes
  // ============================================================================
  
  /** Apply a change to a term */
  def applyChange[A](change: Change[A], term: Term[A]): Term[A] =
    ChangeApplicator(change, term)

  // ============================================================================
  // STAGE 5: Transform via Xform (LC → TypedLC)
  // ============================================================================
  
  /** Transform LC to TypedLC using the TypeChecker Xform */
  def typeCheck(term: Term[LC]): Term[TypedLC] =
    TypeChecker.forward(term)

  /** Transform LC to ICNet */
  def toInteractionNet(term: Term[LC]): Term[ICNet] =
    LCToIC.forward(term)

  // ============================================================================
  // STAGE 6: Rendering
  // ============================================================================
  
  /** Render LC term to string */
  def renderLC(term: Term[LC]): String = term match
    case Term.Done(lc) => renderLCValue(lc)
    case Term.Hole(l)  => l.map(n => s"?$n").getOrElse("?")

  def renderLCValue(lc: LC): String = lc match
    case LC.Var(n)       => n
    case LC.Lam(p, b)    => s"λ$p.${renderLCValue(b)}"
    case LC.App(f, a)    => s"(${renderLCValue(f)} ${renderLCValue(a)})"
    case LC.Let(n, v, b) => s"let $n = ${renderLCValue(v)} in ${renderLCValue(b)}"
    case LC.Lit(n)       => n.toString
    case LC.Prim(op, List(a, b)) if Set("+", "-", "*", "/").contains(op) =>
      s"(${renderLCValue(a)} $op ${renderLCValue(b)})"
    case LC.Prim(op, as) => s"$op(${as.map(renderLCValue).mkString(", ")})"

  /** Render TypedLC term to string with types */
  def renderTypedLC(term: Term[TypedLC]): String = term match
    case Term.Done(typed) => renderTypedLCValue(typed)
    case Term.Hole(l)     => l.map(n => s"?$n").getOrElse("?")

  def renderTypedLCValue(typed: TypedLC): String = typed match
    case TypedLC.TVar(n, ty)       => s"$n:${ty.render}"
    case TypedLC.TLam(p, pty, b)   => s"λ$p:${pty.render}.${renderTypedLCValue(b)}"
    case TypedLC.TApp(f, a)        => s"(${renderTypedLCValue(f)} ${renderTypedLCValue(a)})"
    case TypedLC.TLet(n, ty, v, b) => s"let $n:${ty.render} = ${renderTypedLCValue(v)} in ${renderTypedLCValue(b)}"
    case TypedLC.TLit(n)           => n.toString
    case TypedLC.TAnn(t, ty)       => s"(${renderTypedLCValue(t)} : ${ty.render})"

  // ============================================================================
  // FULL PIPELINE
  // ============================================================================
  
  case class PipelineResult(
    source: String,
    parsed: Term[LC],
    hash: Hash,
    typed: Term[TypedLC],
    icNet: Term[ICNet],
    rendered: String
  )

  def runPipeline(source: String): Either[String, PipelineResult] =
    for
      parsed <- parseLC(source)
    yield
      val repo = new Repo[LC]
      val hash = repo.store(parsed, Set(Name("input")))
      val typed = typeCheck(parsed)
      val icNet = toInteractionNet(parsed)
      val rendered = renderLC(parsed)
      PipelineResult(source, parsed, hash, typed, icNet, rendered)

  // ============================================================================
  // DEMO
  // ============================================================================
  
  def runDemo(): Unit =
    println("═" * 60)
    println("  PHI PIPELINE DEMONSTRATION")
    println("═" * 60)
    println()
    
    val examples = List(
      "λx.x",
      "λf.λx.f x",
      "(λx.x) y"
    )
    
    for source <- examples do
      println(s"Input: $source")
      runPipeline(source) match
        case Right(result) =>
          println(s"  Parsed:   ${result.parsed}")
          println(s"  Hash:     ${result.hash.short}")
          println(s"  Typed:    ${renderTypedLC(result.typed)}")
          println(s"  IC nodes: ${result.icNet match { case Term.Done(net) => net.nodes.size; case _ => 0 }}")
          println(s"  Rendered: ${result.rendered}")
        case Left(err) =>
          println(s"  Error: $err")
      println()
    
    println("═" * 60)
    println("  ROUND-TRIP VERIFICATION")
    println("═" * 60)
    println()
    
    // Verify LC ↔ IC round-trip
    val lcTerm = Term.Done(LC.Lam("x", LC.Var("x")))
    val icNet = LCToIC.forward(lcTerm)
    val lcBack = LCToIC.backward(icNet)
    println(s"LC → IC → LC round-trip:")
    println(s"  Original: ${renderLC(lcTerm)}")
    println(s"  IC nodes: ${icNet match { case Term.Done(net) => net.nodes.size; case _ => 0 }}")
    println(s"  Restored: ${renderLC(lcBack)}")
    println()
    
    // Verify LC ↔ TypedLC round-trip  
    val typed = TypeChecker.forward(lcTerm)
    val erased = TypeChecker.backward(typed)
    println(s"LC → TypedLC → LC round-trip:")
    println(s"  Original: ${renderLC(lcTerm)}")
    println(s"  Typed:    ${renderTypedLC(typed)}")
    println(s"  Erased:   ${renderLC(erased)}")
    println()
    
    println("═" * 60)

  @main def pipelineMain(): Unit = runDemo()

end Pipeline
