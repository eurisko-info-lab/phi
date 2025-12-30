package phi.phi

import phi.meta.*
import phi.meta.Val.*

/**
 * Minimal Grammar Interpreter - parses input according to grammar rules
 */
object GrammarInterp:
  import Lex.*
  
  /** Build a parser from a language spec */
  def specParser(spec: LangSpec): GrammarParser = GrammarParser(spec)

case class GrammarParser(spec: LangSpec):
  import Lex.*
  
  /** Parse input using a named grammar rule */
  def parse(grammarName: String, input: String): Either[String, Val] =
    spec.grammars.get(grammarName) match
      case None => Left(s"Grammar '$grammarName' not found")
      case Some(rules) =>
        val stream = Lexer.tokenize(input).skipWs
        parseRules(rules, stream) match
          case Right((v, _)) => Right(v)
          case Left(e) => Left(e)
  
  private def parseRules(rules: List[SyntaxRule], stream: TokenStream): Either[String, (Val, TokenStream)] =
    rules.foldLeft[Either[String, (Val, TokenStream)]](Left("No rules matched")) { (acc, rule) =>
      acc.orElse(parseRule(rule, stream))
    }
  
  private def parseRule(rule: SyntaxRule, stream: TokenStream): Either[String, (Val, TokenStream)] =
    // Parse the token pattern
    parsePattern(rule.pattern, stream, Map.empty) match
      case Right((bindings, remaining)) =>
        // Build the Val from the result template
        val result = buildVal(rule.result, bindings)
        Right((result, remaining))
      case Left(e) => Left(e)
  
  private def parsePattern(
    pattern: List[SyntaxToken],
    stream: TokenStream,
    bindings: Map[String, Val]
  ): Either[String, (Map[String, Val], TokenStream)] =
    pattern match
      case Nil => Right((bindings, stream))
      case tok :: rest =>
        val s = stream.skipWs
        tok match
          case SyntaxToken.Literal(kw) =>
            s.peek match
              case Ident(id) if id == kw => parsePattern(rest, s.advance, bindings)
              case other => Left(s"Expected keyword '$kw', got $other")
          
          case SyntaxToken.NonTerm("IDENT", _) =>
            s.peek match
              case Ident(id) =>
                val newBindings = bindings + ("IDENT" -> VCon("String", List(VCon(id, Nil))))
                parsePattern(rest, s.advance, newBindings)
              case other => Left(s"Expected identifier, got $other")
          
          case SyntaxToken.NonTerm("STRING", _) =>
            s.peek match
              case StrLit(str) =>
                val newBindings = bindings + ("STRING" -> VCon("String", List(VCon(str, Nil))))
                parsePattern(rest, s.advance, newBindings)
              case other => Left(s"Expected string, got $other")
          
          case SyntaxToken.NonTerm("INT", _) =>
            s.peek match
              case IntLit(n) =>
                val newBindings = bindings + ("INT" -> VCon("Int", List(VCon(n.toString, Nil))))
                parsePattern(rest, s.advance, newBindings)
              case other => Left(s"Expected int, got $other")
          
          case SyntaxToken.NonTerm(name, _) =>
            // Treat as identifier
            s.peek match
              case Ident(id) =>
                val newBindings = bindings + (name -> VCon(id, Nil))
                parsePattern(rest, s.advance, newBindings)
              case other => Left(s"Expected $name, got $other")
  
  /** Build a Val from a template with bindings */
  private def buildVal(template: SyntaxArg, bindings: Map[String, Val]): Val =
    template match
      case SyntaxArg.Ref(name) =>
        bindings.getOrElse(name, VCon(name, Nil))
      
      case SyntaxArg.Con(name, args) =>
        VCon(name, args.map(buildVal(_, bindings)))
      
      case SyntaxArg.Lit(value) =>
        VCon(value, Nil)
      
      case SyntaxArg.StrLit(value) =>
        VCon("String", List(VCon(value, Nil)))
      
      case SyntaxArg.Wrap(wrapper, inner) =>
        VCon(wrapper, List(buildVal(inner, bindings)))
      
      case SyntaxArg.Hole =>
        VCon("?", Nil)

  // ==========================================================================
  // Rendering: Val → String (inverse of parsing)
  // Uses attribute grammar: indent level is inherited attribute
  // ==========================================================================
  
  // Inherited attributes passed during rendering
  case class RenderCtx(indent: Int = 0):
    def indented: RenderCtx = copy(indent = indent + 1)
    def outdented: RenderCtx = copy(indent = math.max(0, indent - 1))
    def indentStr: String = "  " * indent
  
  /** Render a Val to a String using a named grammar */
  def render(grammarName: String, value: Val): String =
    spec.grammars.get(grammarName) match
      case None => s"/* Unknown grammar: $grammarName */"
      case Some(rules) =>
        renderWithRules(rules, value, RenderCtx()).getOrElse(s"/* Cannot render: ${value} */")
  
  private def renderWithRules(rules: List[SyntaxRule], value: Val, ctx: RenderCtx): Option[String] =
    rules.view.flatMap(rule => tryRenderRule(rule, value, ctx)).headOption
  
  private def tryRenderRule(rule: SyntaxRule, value: Val, ctx: RenderCtx): Option[String] =
    matchTemplate(rule.result, value) match
      case Some(bindings) =>
        Some(renderPattern(rule.pattern, bindings, ctx))
      case None => None
  
  /** Match a value against a template, extracting bindings */
  private def matchTemplate(template: SyntaxArg, value: Val): Option[Map[String, Val]] =
    (template, value) match
      case (SyntaxArg.Con(name, args), VCon(vname, vargs)) if name == vname && args.length == vargs.length =>
        // Match constructor, recursively match args
        args.zip(vargs).foldLeft(Option(Map.empty[String, Val])) { case (accOpt, (argTemplate, argVal)) =>
          accOpt.flatMap { acc =>
            matchTemplate(argTemplate, argVal).map(acc ++ _)
          }
        }
      
      case (SyntaxArg.Ref(name), v) =>
        // Bind this value to the name
        Some(Map(name -> v))
      
      case (SyntaxArg.Lit(lit), VCon(name, Nil)) if lit == name =>
        Some(Map.empty)
        
      case (SyntaxArg.StrLit(s), VStr(vs)) if s == vs =>
        Some(Map.empty)
        
      case (SyntaxArg.Wrap(wrapper, inner), VCon(w, List(v))) if wrapper == w =>
        matchTemplate(inner, v)
      
      case _ => None
  
  /** Render a pattern with bindings and inherited attributes */
  private def renderPattern(pattern: List[SyntaxToken], bindings: Map[String, Val], ctx: RenderCtx): String =
    // Thread context through to handle \n+ and \n- indent changes
    val (result, _) = pattern.foldLeft(("", ctx)) { case ((acc, currentCtx), tok) =>
      val (tokStr, newCtx) = renderTokenWithCtx(tok, bindings, currentCtx)
      val combined =
        if acc.isEmpty then tokStr
        else if tokStr.isEmpty then acc                        // \n+/\n- produce empty string
        else if tokStr.startsWith("\n") then acc + tokStr      // newline token starts fresh
        else if acc.endsWith("\n") || acc.matches("(?s).*\\n[ ]*") then acc + tokStr  // after newline+indent
        else if tokStr.matches("^[(),:.].*") then acc + tokStr // punctuation attaches
        else if acc.endsWith("(") then acc + tokStr            // after open paren
        else acc + " " + tokStr                                 // else add space
      (combined, newCtx)
    }
    result
  
  /** Render a token and return updated context (for \n+ and \n-) */
  private def renderTokenWithCtx(tok: SyntaxToken, bindings: Map[String, Val], ctx: RenderCtx): (String, RenderCtx) =
    tok match
      case SyntaxToken.Literal(lit) =>
        lit match
          case _ => (lit, ctx)                                   // Any literal keyword
      
      case SyntaxToken.NonTerm("NL", _) =>
        ("\n" + ctx.indentStr, ctx)                              // Newline + current indent
      
      case SyntaxToken.NonTerm("INDENT", _) =>
        ("", ctx.indented)                                       // Increase indent for following tokens
      
      case SyntaxToken.NonTerm("DEDENT", _) =>
        ("", ctx.outdented)                                      // Decrease indent for following tokens
      
      case SyntaxToken.NonTerm(name, mod) =>
        val str = renderNonTermToken(name, mod, bindings, ctx)
        (str, ctx)
  
  /** Render a non-terminal token from bindings */
  private def renderNonTermToken(name: String, mod: Option[String], bindings: Map[String, Val], ctx: RenderCtx): String =
    // Handle built-in terminals
    if name == "IDENT" then return bindings.get(name).map(extractIdent).getOrElse("???")
    if name == "STRING" then return bindings.get(name).map(v => "\"" + extractIdent(v) + "\"").getOrElse("\"???\"")
    if name == "INT" then return bindings.get(name).map(extractIdent).getOrElse("0")
    
    bindings.get(name) match
      case Some(v) =>
        val items = valToList(v)
        if items.nonEmpty || v.isInstanceOf[VList] || isConsNil(v) then
          // List: render each item with the grammar
          val separator = if name.toLowerCase.contains("expr") || name.toLowerCase.contains("arg") then ", " else ""
          items.map(v => renderNonTerm(name, v, ctx)).mkString(separator)
        else if spec.grammars.contains(name) then
          renderNonTerm(name, v, ctx)
        else
          extractIdent(v)
      case None =>
        s"/* missing: $name */"
  
  private def isConsNil(v: Val): Boolean = v match
    case VCon("Nil", _) => true
    case VCon("Cons", _) => true
    case VCon("List", _) => true
    case VList(_) => true
    case _ => false
  
  private def valToList(v: Val): List[Val] = v match
    case VList(items) => items
    case VCon("Nil", Nil) => Nil
    case VCon("Cons", List(head, tail)) => head :: valToList(tail)
    case VCon("List", items) => items
    case _ => Nil  // Not a list
  
  private def renderNonTerm(grammarName: String, value: Val, ctx: RenderCtx): String =
    spec.grammars.get(grammarName) match
      case Some(rules) => 
        renderWithRules(rules, value, ctx).getOrElse(s"/* cannot render $grammarName: $value */")
      case None => 
        extractIdent(value)
  
  private def extractIdent(v: Val): String = v match
    case VCon("String", List(VCon(s, Nil))) => s
    case VCon(s, Nil) => s
    case VStr(s) => s
    case VInt(n) => n.toString
    case _ => v.toString
