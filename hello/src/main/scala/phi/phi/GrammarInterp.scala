package phi.phi

import phi.meta.*
import phi.meta.Val.*
import TokenRender.RenderCtx

/**
 * Minimal Grammar Interpreter - parses input according to grammar rules
 */
object GrammarInterp:
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
  
  // Use RenderCtx from TokenRender module
  
  /** Render a Val to a String using a named grammar */
  def render(grammarName: String, value: Val): String =
    spec.grammars.get(grammarName) match
      case None => s"/* Unknown grammar: $grammarName */"
      case Some(rules) =>
        renderWithRules(rules, value, RenderCtx()).getOrElse(s"/* Cannot render: ${value} */")
  
  private def renderWithRules(rules: List[SyntaxRule], value: Val, ctx: RenderCtx): Option[String] =
    // Special case: StrLit renders directly as a quoted string
    value match
      case VCon("StrLit", List(VStr(s))) => Some(s"\"$s\"")
      case VCon("StrLit", List(VCon(s, Nil))) => Some(s"\"$s\"")  // String literal as VCon
      case VCon("DQuotLit", Nil) => Some("\"\\\"\"")  // Double quote literal: "\""
      case VCon("RawMember", List(VStr(s))) => Some("\n" + ctx.indentStr + s)  // Raw code with NL prefix
      case VCon("RawMember", List(VCon(s, Nil))) => Some("\n" + ctx.indentStr + s)  // Raw code as VCon with NL
      case _ => rules.view.flatMap(rule => tryRenderRule(rule, value, ctx)).headOption
  
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
      
      // Ref starting with uppercase is a constructor reference - must match exactly
      case (SyntaxArg.Ref(name), VCon(vname, Nil)) if name.headOption.exists(_.isUpper) =>
        if name == vname then Some(Map.empty) else None
      
      // Ref starting with lowercase is a variable binding - match anything
      case (SyntaxArg.Ref(name), v) if name.headOption.exists(_.isLower) =>
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
      (TokenRender.combine(acc, tokStr), newCtx)
    }
    result
  
  /** Render a token and return updated context (for \n+ and \n-) */
  private def renderTokenWithCtx(tok: SyntaxToken, bindings: Map[String, Val], ctx: RenderCtx): (String, RenderCtx) =
    tok match
      case SyntaxToken.Literal(lit) =>
        (lit, ctx)
      
      case SyntaxToken.NonTerm(name, mod) if TokenRender.isBuiltin(name) && !bindings.contains(name) =>
        // Built-in token with special rendering semantics
        TokenRender.renderBuiltin(name, ctx)
      
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
        // If modifier is * or +, treat as list. Otherwise treat as single value.
        val isList = mod.exists(m => m == "*" || m == "+")
        if isList then
          val items = valToList(v)
          // List: render each item with appropriate grammar
          // Use comma separator for expressions, args, and params
          val separator = if name.toLowerCase.contains("expr") || name.toLowerCase.contains("arg") || name.toLowerCase.contains("param") then ", " else ""
          items.map(item =>
            // Special case: StrLit renders as quoted string
            item match
              case VCon("StrLit", List(VStr(s))) => s"\"$s\""
              case VCon("StrLit", List(VCon(s, Nil))) => s"\"$s\""
              case _ =>
                findGrammarFor(item) match
                  case Some(grammarName) => renderNonTerm(grammarName, item, ctx)
                  case None => extractIdent(item)
          ).mkString(separator)
        else if spec.grammars.contains(name) then
          renderNonTerm(name, v, ctx)
        else
          // Single value: try to find a grammar or extract ident
          findGrammarFor(v) match
            case Some(grammarName) => 
              renderNonTerm(grammarName, v, ctx)
            case None => 
              extractIdent(v)
      case None =>
        s"/* missing: $name */"
  
  /** Find which grammar can render a given value */
  private def findGrammarFor(v: Val): Option[String] =
    v match
      case VCon(conName, _) =>
        // Look through all grammars to find one with a rule for this constructor
        spec.grammars.find { case (grammarName, rules) =>
          rules.exists(rule => rule.result match
            case SyntaxArg.Con(name, _) => name == conName
            case SyntaxArg.Ref(name) => name == conName && name.headOption.exists(_.isUpper)
            case _ => false
          )
        }.map(_._1)
      case _ => None
  
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
