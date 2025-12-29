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
    def parseExpr(): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input")
      else parseOr()
    
    // Or = And ('|' And)*
    private def parseOr(): Either[String, Term[LC]] =
      parseAnd().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if !atEnd && peek == '|' then
            advance()
            parseAnd() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("|", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // And = Comparison ('&' Comparison)*
    private def parseAnd(): Either[String, Term[LC]] =
      parseComparison().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if !atEnd && peek == '&' then
            advance()
            parseComparison() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("&", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Comparison = Additive (('=' | '!=' | '<' | '>' | '<=' | '>=') Additive)?
    private def parseComparison(): Either[String, Term[LC]] =
      parseAdditive().flatMap { left =>
        skipWs()
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
            parseAdditive().map { right =>
              Term.Done(LC.Prim(o, List(left.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?")))))
            }
          case None => Right(left)
      }
    
    // Additive = Multiplicative (('+' | '-') Multiplicative)*
    private def parseAdditive(): Either[String, Term[LC]] =
      parseMultiplicative().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if !atEnd && (peek == '+' || peek == '-') then
            val op = advance().toString
            parseMultiplicative() match
              case Right(right) =>
                loop(Term.Done(LC.Prim(op, List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Multiplicative = Application (('*' | '/') Application)*
    private def parseMultiplicative(): Either[String, Term[LC]] =
      parseApplication().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if !atEnd && (peek == '*' || peek == '/') then
            val op = advance().toString
            parseApplication() match
              case Right(right) =>
                loop(Term.Done(LC.Prim(op, List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    // Application = Atom+
    private def parseApplication(): Either[String, Term[LC]] =
      parseAtom().flatMap { first =>
        var result = first
        var continue = true
        while continue do
          skipWs()
          if !atEnd && isAtomStart(peek) then
            parseAtom() match
              case Right(arg) =>
                result = Term.Done(LC.App(result.getOrElse(LC.Var("?")), arg.getOrElse(LC.Var("?"))))
              case Left(_) => continue = false
          else continue = false
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
    
    // Parse expression for if condition/branches - stops at then/else keywords
    private def parseIfPart(): Either[String, Term[LC]] =
      parseIfOr()
    
    private def parseIfOr(): Either[String, Term[LC]] =
      parseIfAnd().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || lookingAtKeyword() then Right(result)
          else if peek == '|' then
            advance()
            parseIfAnd() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("|", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    private def parseIfAnd(): Either[String, Term[LC]] =
      parseIfComparison().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || lookingAtKeyword() then Right(result)
          else if peek == '&' then
            advance()
            parseIfComparison() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("&", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    private def parseIfComparison(): Either[String, Term[LC]] =
      parseIfAdditive().flatMap { left =>
        skipWs()
        if atEnd || lookingAtKeyword() then Right(left)
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
              parseIfAdditive().map { right =>
                Term.Done(LC.Prim(o, List(left.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?")))))
              }
            case None => Right(left)
      }
    
    private def parseIfAdditive(): Either[String, Term[LC]] =
      parseIfMultiplicative().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || lookingAtKeyword() then Right(result)
          else if peek == '+' || peek == '-' then
            val op = advance().toString
            parseIfMultiplicative() match
              case Right(right) =>
                loop(Term.Done(LC.Prim(op, List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    private def parseIfMultiplicative(): Either[String, Term[LC]] =
      parseIfApplication().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd || lookingAtKeyword() then Right(result)
          else if peek == '*' || peek == '/' then
            val op = advance().toString
            parseIfApplication() match
              case Right(right) =>
                loop(Term.Done(LC.Prim(op, List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(e) => Left(e)
          else Right(result)
        loop(left)
      }
    
    private def parseIfApplication(): Either[String, Term[LC]] =
      parseIfAtom().flatMap { first =>
        var result = first
        var continue = true
        while continue do
          skipWs()
          if atEnd || lookingAtKeyword() || !isAtomStart(peek) then continue = false
          else
            parseIfAtom() match
              case Right(arg) =>
                result = Term.Done(LC.App(result.getOrElse(LC.Var("?")), arg.getOrElse(LC.Var("?"))))
              case Left(_) => continue = false
        Right(result)
      }
    
    private def parseIfAtom(): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input in if")
      else peek match
        case '(' =>
          advance()
          val result = parseExpr()
          skipWs()
          if peek == ')' then advance()
          result
        case c if c.isDigit =>
          Right(Term.Done(LC.Lit(parseNumber())))
        case c if c.isLetter =>
          val savedPos = pos
          val name = parseIdent()
          if isKeyword(name) then
            pos = savedPos
            Left("Unexpected keyword in if expression")
          else
            Right(Term.Done(LC.Var(name)))
        case _ =>
          Left(s"Unexpected character in if: $peek")

    // Parse expression for let value - stops at 'in' keyword
    private def parseLetValue(): Either[String, Term[LC]] =
      parseLetAdditive()
    
    private def parseLetAdditive(): Either[String, Term[LC]] =
      parseLetMultiplicative().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd then Right(result)
          else if peek == '+' then
            advance()
            parseLetMultiplicative() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("+", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(err) => Left(err)
          else if peek == '-' then
            advance()
            parseLetMultiplicative() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("-", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(err) => Left(err)
          else Right(result)
        loop(left)
      }
    
    private def parseLetMultiplicative(): Either[String, Term[LC]] =
      parseLetApplication().flatMap { left =>
        @tailrec def loop(result: Term[LC]): Either[String, Term[LC]] =
          skipWs()
          if atEnd then Right(result)
          else if peek == '*' then
            advance()
            parseLetApplication() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("*", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(err) => Left(err)
          else if peek == '/' then
            advance()
            parseLetApplication() match
              case Right(right) =>
                loop(Term.Done(LC.Prim("/", List(result.getOrElse(LC.Var("?")), right.getOrElse(LC.Var("?"))))))
              case Left(err) => Left(err)
          else Right(result)
        loop(left)
      }
    
    private def parseLetApplication(): Either[String, Term[LC]] =
      parseLetAtom().flatMap { first =>
        var result = first
        var continue = true
        while continue do
          skipWs()
          if !atEnd && isLetAtomStart then
            parseLetAtom() match
              case Right(arg) =>
                result = Term.Done(LC.App(result.getOrElse(LC.Var("?")), arg.getOrElse(LC.Var("?"))))
              case Left(_) => continue = false
          else continue = false
        Right(result)
      }
    
    // Check if next token is an atom (not 'in')
    private def isLetAtomStart: Boolean =
      val c = peek
      if c.isLetter then
        // Check if it's the 'in' keyword - look ahead
        val saved = pos
        val ident = parseIdent()
        pos = saved
        ident != "in"
      else c.isDigit || c == '(' || c == 'λ' || c == '\\' || c == '?'
    
    private def parseLetAtom(): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input")
      else peek match
        case 'λ' | '\\' =>
          advance()
          skipWs()
          val param = parseIdent()
          skipWs()
          if peek == '.' then advance()
          else if peek2 == "->" || peek2 == "=>" then { advance(); advance() }
          parseLetValue().map(body => Term.Done(LC.Lam(param, body.getOrElse(LC.Var("?")))))
        
        case '(' =>
          advance()
          val result = parseExpr()  // Inside parens, can use full expr
          skipWs()
          if peek == ')' then advance()
          result
        
        case '?' =>
          advance()
          val label = if !atEnd && peek.isLetter then Some(parseIdent()) else None
          Right(Term.Hole(label))
        
        case c if c.isDigit =>
          Right(Term.Done(LC.Lit(parseNumber())))
        
        case c if c.isLetter =>
          val name = parseIdent()
          name match
            case "in" =>
              // Put it back - let caller handle it
              pos -= 2
              Left("Reached 'in' keyword")
            case "fn" | "fun" =>
              skipWs()
              val param = parseIdent()
              skipWs()
              expect("=>") || expect("->")
              parseLetValue().map(body => Term.Done(LC.Lam(param, body.getOrElse(LC.Var("?")))))
            case _ =>
              Right(Term.Done(LC.Var(name)))
        
        case c =>
          Left(s"Unexpected character: $c")

    // Atom = Var | Lit | '(' Expr ')' | Lambda | Let | If | Hole
    private def parseAtom(): Either[String, Term[LC]] =
      skipWs()
      if atEnd then Left("Unexpected end of input")
      else peek match
        // Lambda: λx.body or \x.body
        case 'λ' | '\\' =>
          advance()
          skipWs()
          val param = parseIdent()
          skipWs()
          if peek == '.' then advance()
          else if peek2 == "->" || peek2 == "=>" then { advance(); advance() }
          parseExpr().map(body => Term.Done(LC.Lam(param, body.getOrElse(LC.Var("?")))))
        
        // Parenthesized expression
        case '(' =>
          advance()
          val result = parseExpr()
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
          val name = parseIdent()
          name match
            case "fn" | "fun" =>
              skipWs()
              val param = parseIdent()
              skipWs()
              expect("=>") || expect("->")
              parseExpr().map(body => Term.Done(LC.Lam(param, body.getOrElse(LC.Var("?")))))
            
            case "let" =>
              skipWs()
              val varName = parseIdent()
              skipWs()
              expect("=")
              for
                value <- parseLetValue()  // Parse value stopping at 'in'
                _ = { skipWs(); expect("in") }
                body <- parseExpr()
              yield Term.Done(LC.Let(varName, value.getOrElse(LC.Var("?")), body.getOrElse(LC.Var("?"))))
            
            case "if" =>
              skipWs()
              for
                cond <- parseIfPart()  // Stop at then/else
                _ = { skipWs(); expect("then") }
                thenBr <- parseIfPart()  // Stop at then/else
                _ = { skipWs(); expect("else") }
                elseBr <- parseExpr()  // Rest can be full expr
              yield Term.Done(LC.Prim("if", List(
                cond.getOrElse(LC.Var("?")),
                thenBr.getOrElse(LC.Var("?")),
                elseBr.getOrElse(LC.Var("?"))
              )))
            
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
