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
  // ==========================================================================
  
  /** Render a Val to a String using a named grammar */
  def render(grammarName: String, value: Val): String =
    spec.grammars.get(grammarName) match
      case None => s"/* Unknown grammar: $grammarName */"
      case Some(rules) =>
        renderWithRules(rules, value).getOrElse(s"/* Cannot render: ${value} */")
  
  private def renderWithRules(rules: List[SyntaxRule], value: Val): Option[String] =
    rules.view.flatMap(rule => tryRenderRule(rule, value)).headOption
  
  private def tryRenderRule(rule: SyntaxRule, value: Val): Option[String] =
    // Try to match the value against the rule's result template
    matchTemplate(rule.result, value) match
      case Some(bindings) =>
        // Render the pattern with bindings
        Some(renderPattern(rule.pattern, bindings))
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
  
  /** Render a pattern with bindings */
  private def renderPattern(pattern: List[SyntaxToken], bindings: Map[String, Val]): String =
    pattern.map(renderToken(_, bindings)).mkString(" ")
  
  private def renderToken(tok: SyntaxToken, bindings: Map[String, Val]): String =
    tok match
      case SyntaxToken.Literal(kw) => kw
      
      case SyntaxToken.NonTerm("IDENT", _) =>
        bindings.get("IDENT").map(extractIdent).getOrElse("???")
      
      case SyntaxToken.NonTerm("STRING", _) =>
        bindings.get("STRING").map(v => "\"" + extractIdent(v) + "\"").getOrElse("\"???\"")
      
      case SyntaxToken.NonTerm("INT", _) =>
        bindings.get("INT").map(extractIdent).getOrElse("0")
      
      case SyntaxToken.NonTerm(name, mod) =>
        bindings.get(name) match
          case Some(VList(items)) =>
            // Render each item and join
            items.map(v => renderNonTerm(name, v)).mkString(" ")
          case Some(v) =>
            // Check if there's a grammar for this, otherwise treat as identifier
            if spec.grammars.contains(name) then
              renderNonTerm(name, v)
            else
              extractIdent(v)  // Treat as identifier terminal
          case None =>
            s"/* missing: $name */"
  
  private def renderNonTerm(grammarName: String, value: Val): String =
    // Look for grammar with exact name
    spec.grammars.get(grammarName) match
      case Some(rules) => 
        renderWithRules(rules, value).getOrElse(s"/* cannot render $grammarName: $value */")
      case None => 
        extractIdent(value)
  
  private def extractIdent(v: Val): String = v match
    case VCon("String", List(VCon(s, Nil))) => s
    case VCon(s, Nil) => s
    case VStr(s) => s
    case VInt(n) => n.toString
    case _ => v.toString
