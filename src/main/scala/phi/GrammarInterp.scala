package phi

import Val.*

/**
 * Bidirectional Grammar Interpreter
 * 
 * Interprets grammar rules from .phi files to create Syntax[Val] instances
 * that can both parse text → AST and print AST → text.
 */
object GrammarInterp:
  
  /**
   * Build a Syntax[Val] from grammar rules for a specific sort.
   * The grammar rules define bidirectional transformations.
   */
  def buildSyntax(grammarName: String, rules: List[SyntaxRule]): Syntax[Val] =
    if rules.isEmpty then
      Syntax.fail(s"no rules for grammar $grammarName")
    else
      rules.map(ruleToSyntax).reduce(_ | _)
  
  /**
   * Convert a single grammar rule to a Syntax[Val].
   * 
   * Example rule: "sealed" "trait" IDENT typeParam* => SealedTrait
   * 
   * Parses:  "sealed trait Foo" → VCon("SealedTrait", [VCon("Foo", []), VCon("nil", [])])
   * Renders: VCon("SealedTrait", [name, tps]) → "sealed trait " ++ name ++ tps
   */
  def ruleToSyntax(rule: SyntaxRule): Syntax[Val] =
    // Build syntax for each token, collecting non-terminal bindings
    val (tokenSyntaxes, bindings) = buildTokenSyntaxes(rule.tokens)
    
    if tokenSyntaxes.isEmpty then
      return Syntax.pure(VCon(rule.constructor, Nil))
    
    // Combine all token syntaxes into a sequence that collects values
    val combined: Syntax[List[Val]] = tokenSyntaxes match
      case Nil => Syntax.pure(Nil)
      case head :: Nil => head.iso(Iso(v => List(v), _.head))
      case head :: tail =>
        tail.foldLeft(head.iso(Iso[Val, List[Val]](v => List(v), _.head))) { (acc, next) =>
          (acc ~ next).iso(Iso(
            { case (list, v) => list :+ v },
            { case list if list.nonEmpty => (list.init, list.last)
              case _ => (Nil, VCon("unit", Nil)) }
          ))
        }
    
    // Apply the constructor transformation
    combined.iso(Iso(
      // Parse direction: build VCon from captured values
      { values =>
        val args = if rule.args.isEmpty then
          // No explicit args - use all captured values in order
          values.filterNot(_ == VCon("unit", Nil))
        else
          // Explicit args - resolve references
          def resolveArg(arg: SyntaxArg): Val = arg match
            case SyntaxArg.Hole => VCon("_", Nil)
            case SyntaxArg.Ref(name) => 
              bindings.get(name).flatMap(idx => values.lift(idx)).getOrElse(VCon(name, Nil))
            case SyntaxArg.Lit("nil") => VCon("nil", Nil)
            case SyntaxArg.Lit("none") => VCon("none", Nil)
            case SyntaxArg.Lit(other) => VCon(other, Nil)
            case SyntaxArg.StrLit(s) => VCon("str", List(VCon(s, Nil)))
            case SyntaxArg.Wrap(wrapper, inner) => VCon(wrapper, List(resolveArg(inner)))
            case SyntaxArg.Con(name, innerArgs) => VCon(name, innerArgs.map(resolveArg))
          rule.args.map(resolveArg)
        VCon(rule.constructor, args)
      },
      // Render direction: extract values from VCon
      { 
        case VCon(name, args) if name == rule.constructor =>
          // Reconstruct the value list for rendering
          val result = Array.fill[Val](tokenSyntaxes.length)(VCon("unit", Nil))
          if rule.args.isEmpty then
            // No explicit args - distribute args to non-terminals
            val nonTermIndices = bindings.values.toList.sorted
            args.zip(nonTermIndices).foreach { case (arg, idx) => result(idx) = arg }
          else
            // Explicit args - put back by reference
            rule.args.zip(args).foreach {
              case (SyntaxArg.Ref(name), arg) =>
                bindings.get(name).foreach(idx => result(idx) = arg)
              case _ => ()
            }
          result.toList
        case v => 
          // Constructor doesn't match - signal to try next alternative
          throw new Syntax.RenderMismatch
      }
    ))
  
  /**
   * Build Syntax for each token, tracking non-terminal bindings.
   * Returns (list of Syntax[Val], map of name → index)
   */
  private def buildTokenSyntaxes(tokens: List[SyntaxToken]): (List[Syntax[Val]], Map[String, Int]) =
    val syntaxes = List.newBuilder[Syntax[Val]]
    val bindings = Map.newBuilder[String, Int]
    var idx = 0
    
    tokens.foreach { tok =>
      val syn = tok match
        case SyntaxToken.Literal(s) =>
          // Literal: parse/render exact text
          literalSyntax(s)
        
        case SyntaxToken.NonTerm(name, None) =>
          // Simple non-terminal: parse/render a value
          bindings += (name -> idx)
          nonTermSyntax(name)
        
        case SyntaxToken.NonTerm(name, Some("*")) =>
          // List non-terminal: parse/render zero or more
          bindings += (name -> idx)
          listSyntax(name)
        
        case SyntaxToken.NonTerm(name, Some("+")) =>
          // Non-empty list non-terminal
          bindings += (name -> idx)
          list1Syntax(name)
        
        case SyntaxToken.NonTerm(name, Some(mod)) =>
          // Unknown modifier
          bindings += (name -> idx)
          nonTermSyntax(name)
      
      syntaxes += syn
      idx += 1
    }
    
    (syntaxes.result(), bindings.result())
  
  /**
   * Syntax for a literal keyword/symbol.
   * Always produces VCon("unit", Nil) on parse.
   */
  def literalSyntax(text: String): Syntax[Val] = new Syntax[Val]:
    def parse(stream: TokenStream): ParseResult[Val] =
      val s = stream.skipWs
      // Check for keyword or symbol match
      s.peek match
        case Lex.Keyword(k) if k == text => 
          ParseResult.done(VCon("unit", Nil), s.advance)
        case Lex.Symbol(sym) if sym == text =>
          ParseResult.done(VCon("unit", Nil), s.advance)
        case Lex.Ident(id) if id == text =>
          ParseResult.done(VCon("unit", Nil), s.advance)
        case _ =>
          // Try matching the text directly
          val remaining = s.remaining.map(_.render).mkString
          if remaining.startsWith(text) then
            // Advance by consuming the literal
            var advances = 0
            var consumed = 0
            var current = s
            while consumed < text.length && current.nonEmpty do
              val tok = current.peek
              val tokLen = tok.render.length
              if consumed + tokLen <= text.length then
                consumed += tokLen
                advances += 1
                current = current.advance
              else
                consumed = text.length // force exit
            ParseResult.done(VCon("unit", Nil), s.advance(advances))
          else
            ParseResult.hole(stream, Some(s"expected '$text'"))
    
    def render(term: Term[Val]): Vector[Lex] =
      // Determine if it's a keyword or symbol
      if text.forall(_.isLetter) then
        Vector(Lex.Keyword(text), Lex.Whitespace(" "))
      else
        Vector(Lex.Symbol(text))
  
  /**
   * Syntax for a simple non-terminal (IDENT, NAT, etc.)
   */
  def nonTermSyntax(name: String): Syntax[Val] = name.toUpperCase match
    case "IDENT" | "NAME" => identSyntax
    case "NAT" | "INT" | "NUMBER" => natSyntax
    case "STRING" => stringSyntax
    case _ => 
      // Assume it's a reference to another grammar - use ident for now
      identSyntax
  
  /** Parse/render an identifier as VCon(name, Nil) */
  val identSyntax: Syntax[Val] = new Syntax[Val]:
    def parse(stream: TokenStream): ParseResult[Val] =
      stream.skipWs.peek match
        case Lex.Ident(name) => 
          ParseResult.done(VCon(name, Nil), stream.skipWs.advance)
        case _ => 
          ParseResult.hole(stream, Some("expected identifier"))
    
    def render(term: Term[Val]): Vector[Lex] = term match
      case Term.Done(VCon(name, Nil)) => Vector(Lex.Ident(name), Lex.Whitespace(" "))
      case Term.Done(VCon(name, _)) => Vector(Lex.Ident(name), Lex.Whitespace(" "))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))
      case _ => Vector()
  
  /** Parse/render a number as VCon(n.toString, Nil) */
  val natSyntax: Syntax[Val] = new Syntax[Val]:
    def parse(stream: TokenStream): ParseResult[Val] =
      stream.skipWs.peek match
        case Lex.IntLit(n) => 
          ParseResult.done(VCon(n.toString, Nil), stream.skipWs.advance)
        case _ => 
          ParseResult.hole(stream, Some("expected number"))
    
    def render(term: Term[Val]): Vector[Lex] = term match
      case Term.Done(VCon(n, Nil)) => 
        n.toIntOption.map(i => Vector(Lex.IntLit(i))).getOrElse(Vector())
      case Term.Hole(l) => Vector(Lex.HoleTok(l))
      case _ => Vector()
  
  /** Parse/render a string literal */
  val stringSyntax: Syntax[Val] = new Syntax[Val]:
    def parse(stream: TokenStream): ParseResult[Val] =
      stream.skipWs.peek match
        case Lex.StringLit(s) => 
          ParseResult.done(VCon(s, Nil), stream.skipWs.advance)
        case _ => 
          ParseResult.hole(stream, Some("expected string"))
    
    def render(term: Term[Val]): Vector[Lex] = term match
      case Term.Done(VCon(s, Nil)) => Vector(Lex.StringLit(s))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))
      case _ => Vector()
  
  /**
   * Syntax for a list non-terminal (name*).
   * Parses/renders as VCon("nil", []) or VCon("cons", [head, tail])
   */
  def listSyntax(elemName: String): Syntax[Val] = new Syntax[Val]:
    val elemSyn = nonTermSyntax(elemName)
    
    def parse(stream: TokenStream): ParseResult[Val] =
      var current = stream
      var items = List.empty[Val]
      var continue = true
      while continue do
        val r = elemSyn.parse(current)
        r.term match
          case Term.Done(v) if v != VCon("unit", Nil) =>
            items = items :+ v
            current = r.remaining
          case _ =>
            continue = false
      ParseResult.done(listToVal(items), current)
    
    def render(term: Term[Val]): Vector[Lex] = term match
      case Term.Done(v) =>
        valToList(v).flatMap(elem => elemSyn.render(Term.Done(elem))).toVector
      case Term.Hole(l) => Vector(Lex.HoleTok(l))
  
  /**
   * Syntax for a non-empty list non-terminal (name+).
   */
  def list1Syntax(elemName: String): Syntax[Val] = new Syntax[Val]:
    val listSyn = listSyntax(elemName)
    
    def parse(stream: TokenStream): ParseResult[Val] =
      val r = listSyn.parse(stream)
      r.term match
        case Term.Done(VCon("nil", Nil)) =>
          ParseResult.hole(stream, Some(s"expected at least one $elemName"))
        case _ => r
    
    def render(term: Term[Val]): Vector[Lex] = listSyn.render(term)
  
  /** Convert List[Val] to VCon("cons", ...) / VCon("nil", []) */
  def listToVal(items: List[Val]): Val =
    items.foldRight(VCon("nil", Nil): Val) { (h, t) => VCon("cons", List(h, t)) }
  
  /** Convert VCon list to List[Val] */
  def valToList(v: Val): List[Val] = v match
    case VCon("nil", Nil) => Nil
    case VCon("cons", List(h, t)) => h :: valToList(t)
    case VCon("nil", items) => items  // Alternative list representation
    case _ => Nil

  // =========================================================================
  // High-level API
  // =========================================================================
  
  /**
   * Create a printer for a specific grammar from a LangSpec.
   */
  def printer(spec: LangSpec, grammarName: String): Option[Val => String] =
    spec.grammars.get(grammarName).map { rules =>
      val syntax = buildSyntax(grammarName, rules)
      (v: Val) => Renderer.render(syntax, Term.Done(v))
    }
  
  /**
   * Create a parser for a specific grammar from a LangSpec.
   */
  def parser(spec: LangSpec, grammarName: String): Option[String => Term[Val]] =
    spec.grammars.get(grammarName).map { rules =>
      val syntax = buildSyntax(grammarName, rules)
      (input: String) => {
        val tokens = Lexer.tokenize(input)
        syntax.parse(tokens).term
      }
    }
  
  /**
   * Print a Val using the most specific matching grammar rule.
   * Tries each rule until one matches the constructor.
   */
  def printVal(rules: List[SyntaxRule], v: Val): String = v match
    case VCon(name, args) =>
      rules.find(_.constructor == name) match
        case Some(rule) =>
          val syntax = ruleToSyntax(rule)
          Renderer.render(syntax, Term.Done(v))
        case None =>
          // Fallback: print as constructor application
          if args.isEmpty then name
          else s"$name(${args.map(a => printVal(rules, a)).mkString(", ")})"
    case _ => v.toString
  // =========================================================================
  // SpecParser - Full parser environment from a LangSpec
  // =========================================================================
  
  /**
   * A parser environment that resolves grammar references.
   * 
   * When a non-terminal like `expr` appears in a rule, it looks up
   * the `expr` grammar from the spec and delegates to it.
   */
  class SpecParser(spec: LangSpec):
    // Memoized parsers for each grammar (lazily initialized)
    private val parsers = collection.mutable.Map[String, Syntax[Val]]()
    // Track grammars currently being built to detect recursion
    private val building = collection.mutable.Set[String]()
    
    /** Get or build the parser for a grammar */
    def grammarParser(name: String): Syntax[Val] =
      parsers.get(name) match
        case Some(syn) => syn
        case None =>
          // Return a lazy reference to avoid infinite recursion
          lazyRef(name)
    
    /** Lazy reference that defers grammar lookup */
    private def lazyRef(name: String): Syntax[Val] = new Syntax[Val]:
      def parse(stream: TokenStream): ParseResult[Val] =
        getOrBuild(name).parse(stream)
      def render(term: Term[Val]): Vector[Lex] =
        getOrBuild(name).render(term)
    
    /** Actually build the grammar (called lazily) */
    private def getOrBuild(name: String): Syntax[Val] =
      parsers.getOrElseUpdate(name, buildGrammarParser(name))
    
    private def buildGrammarParser(name: String): Syntax[Val] =
      spec.grammars.get(name) match
        case Some(rules) => 
          // Build syntax with grammar-aware non-terminal resolution
          if rules.isEmpty then Syntax.fail(s"empty grammar: $name")
          else rules.map(ruleToSyntaxWithRefs).reduce(_ | _)
        case None =>
          // Fallback to simple identifier
          identSyntax
    
    /** Convert a rule to Syntax, with grammar references resolved */
    private def ruleToSyntaxWithRefs(rule: SyntaxRule): Syntax[Val] =
      val (tokenSyntaxes, bindings) = buildTokenSyntaxesWithRefs(rule.tokens)
      
      if tokenSyntaxes.isEmpty then
        return Syntax.pure(VCon(rule.constructor, Nil))
      
      val combined: Syntax[List[Val]] = tokenSyntaxes match
        case Nil => Syntax.pure(Nil)
        case head :: Nil => head.iso(Iso(v => List(v), _.head))
        case head :: tail =>
          tail.foldLeft(head.iso(Iso[Val, List[Val]](v => List(v), _.head))) { (acc, next) =>
            (acc ~ next).iso(Iso(
              { case (list, v) => list :+ v },
              { case list if list.nonEmpty => (list.init, list.last)
                case _ => (Nil, VCon("unit", Nil)) }
            ))
          }
      
      combined.iso(Iso(
        { values =>
          val args = if rule.args.isEmpty then
            values.filterNot(_ == VCon("unit", Nil))
          else
            def resolveArg(arg: SyntaxArg): Val = arg match
              case SyntaxArg.Hole => VCon("_", Nil)
              case SyntaxArg.Ref(name) => 
                bindings.get(name).flatMap(idx => values.lift(idx)).getOrElse(VCon(name, Nil))
              case SyntaxArg.Lit("nil") => VCon("nil", Nil)
              case SyntaxArg.Lit("none") => VCon("none", Nil)
              case SyntaxArg.Lit(other) => VCon(other, Nil)
              case SyntaxArg.StrLit(s) => VCon("str", List(VCon(s, Nil)))
              case SyntaxArg.Wrap(wrapper, inner) => VCon(wrapper, List(resolveArg(inner)))
              case SyntaxArg.Con(name, innerArgs) => VCon(name, innerArgs.map(resolveArg))
            rule.args.map(resolveArg)
          VCon(rule.constructor, args)
        },
        { 
          case VCon(name, args) if name == rule.constructor =>
            val result = Array.fill[Val](tokenSyntaxes.length)(VCon("unit", Nil))
            if rule.args.isEmpty then
              val nonTermIndices = bindings.values.toList.sorted
              args.zip(nonTermIndices).foreach { case (arg, idx) => result(idx) = arg }
            else
              rule.args.zip(args).foreach {
                case (SyntaxArg.Ref(name), arg) =>
                  bindings.get(name).foreach(idx => result(idx) = arg)
                case _ => ()
              }
            result.toList
          case v => throw new Syntax.RenderMismatch
        }
      ))
    
    /** Build token syntaxes with grammar reference support */
    private def buildTokenSyntaxesWithRefs(tokens: List[SyntaxToken]): (List[Syntax[Val]], Map[String, Int]) =
      val syntaxes = List.newBuilder[Syntax[Val]]
      val bindings = Map.newBuilder[String, Int]
      var idx = 0
      
      tokens.foreach { tok =>
        val syn = tok match
          case SyntaxToken.Literal(s) =>
            literalSyntax(s)
          
          case SyntaxToken.NonTerm(name, None) =>
            bindings += (name -> idx)
            resolveNonTerm(name, false)
          
          case SyntaxToken.NonTerm(name, Some("*")) =>
            bindings += (name -> idx)
            resolveNonTerm(name, true)
          
          case SyntaxToken.NonTerm(name, Some("+")) =>
            bindings += (name -> idx)
            resolveNonTermPlus(name)
          
          case SyntaxToken.NonTerm(name, Some(_)) =>
            bindings += (name -> idx)
            resolveNonTerm(name, false)
        
        syntaxes += syn
        idx += 1
      }
      
      (syntaxes.result(), bindings.result())
    
    /** Resolve a non-terminal: either a builtin or a grammar reference */
    private def resolveNonTerm(name: String, isList: Boolean): Syntax[Val] =
      name.toUpperCase match
        case "IDENT" | "NAME" => if isList then wrapList(identSyntax) else identSyntax
        case "NAT" | "INT" | "NUMBER" => if isList then wrapList(natSyntax) else natSyntax
        case "STRING" => if isList then wrapList(stringSyntax) else stringSyntax
        case "CTOR" => if isList then wrapList(ctorSyntax) else ctorSyntax
        case _ =>
          // Check if it's a grammar name
          if spec.grammars.contains(name) then
            if isList then wrapList(grammarParser(name))
            else grammarParser(name)
          else if spec.grammars.contains(name.toLowerCase) then
            if isList then wrapList(grammarParser(name.toLowerCase))
            else grammarParser(name.toLowerCase)
          else
            // Fall back to identifier
            if isList then wrapList(identSyntax) else identSyntax
    
    private def resolveNonTermPlus(name: String): Syntax[Val] =
      val base = resolveNonTerm(name, false)
      list1SyntaxFrom(base)
    
    /** Wrap a syntax to parse/render as a list */
    private def wrapList(elem: Syntax[Val]): Syntax[Val] = new Syntax[Val]:
      def parse(stream: TokenStream): ParseResult[Val] =
        var current = stream
        var items = List.empty[Val]
        var continue = true
        while continue do
          val r = elem.parse(current)
          r.term match
            case Term.Done(v) if v != VCon("unit", Nil) =>
              items = items :+ v
              current = r.remaining
            case _ =>
              continue = false
        ParseResult.done(listToVal(items), current)
      
      def render(term: Term[Val]): Vector[Lex] = term match
        case Term.Done(v) =>
          valToList(v).flatMap(e => elem.render(Term.Done(e))).toVector
        case Term.Hole(l) => Vector(Lex.HoleTok(l))
    
    private def list1SyntaxFrom(elem: Syntax[Val]): Syntax[Val] = new Syntax[Val]:
      val listSyn = wrapList(elem)
      def parse(stream: TokenStream): ParseResult[Val] =
        val r = listSyn.parse(stream)
        r.term match
          case Term.Done(VCon("nil", Nil)) =>
            ParseResult.hole(stream, Some("expected at least one element"))
          case _ => r
      def render(term: Term[Val]): Vector[Lex] = listSyn.render(term)
    
    /** Parse constructor names (capitalized identifiers) */
    val ctorSyntax: Syntax[Val] = new Syntax[Val]:
      def parse(stream: TokenStream): ParseResult[Val] =
        stream.skipWs.peek match
          case Lex.Ident(name) if name.headOption.exists(_.isUpper) =>
            ParseResult.done(VCon(name, Nil), stream.skipWs.advance)
          case _ =>
            ParseResult.hole(stream, Some("expected constructor (capitalized identifier)"))
      def render(term: Term[Val]): Vector[Lex] = term match
        case Term.Done(VCon(name, Nil)) => Vector(Lex.Ident(name), Lex.Whitespace(" "))
        case Term.Hole(l) => Vector(Lex.HoleTok(l))
        case _ => Vector()
    
    /** Parse input using a specific grammar */
    def parse(grammarName: String, input: String): Term[Val] =
      val tokens = Lexer.tokenize(input)
      grammarParser(grammarName).parse(tokens).term
    
    /** Render a value using a specific grammar */
    def render(grammarName: String, value: Val): String =
      Renderer.render(grammarParser(grammarName), Term.Done(value))
  
  /** Create a SpecParser from a LangSpec */
  def specParser(spec: LangSpec): SpecParser = new SpecParser(spec)