package phi

import Val.*

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
