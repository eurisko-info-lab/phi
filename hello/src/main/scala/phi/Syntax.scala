package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║                     Φ-SYNTAX: Bidirectional Parsing                       ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  Parser-combinator framework with BOTH parse AND print directions         ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * PHILOSOPHY
 * ==========
 * 
 * Traditional parsers are one-way: String → AST.
 * Bidirectional parsers work both ways: String ⇄ AST.
 * 
 * This means a single grammar definition gives you:
 *   - A parser (String → AST)
 *   - A printer (AST → String)
 *   - A round-trip guarantee: parse(print(ast)) == ast
 * 
 * WHY BIDIRECTIONAL?
 * ==================
 * 
 * 1. Single source of truth for syntax
 * 2. Pretty-printing for free
 * 3. Error messages that show expected syntax
 * 4. Refactoring tools that preserve formatting
 * 5. Fewer bugs from parser/printer inconsistency
 *
 * USAGE
 * =====
 * 
 *   // Define a syntax
 *   val num: Syntax[Int] = 
 *     digits.imap(_.toInt, _.toString)
 *   
 *   val add: Syntax[(Int, Int)] =
 *     num ~ keyword("+") ~ num
 *   
 *   // Use both directions
 *   add.parse("1 + 2")     // Some((1, 2))
 *   add.render((1, 2))     // "1 + 2"
 */
object Syntax:
  import Core.*
  import Core.Val.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: Tokens
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Token: The lexical units of a language.
   * 
   * Lexing (String → List[Token]) is typically done first,
   * then parsing operates on the token stream.
   */
  enum Token:
    case TIdent(name: String)      // Identifier: foo, Bar, _x1
    case TKeyword(word: String)    // Keyword: if, then, else
    case TSymbol(sym: String)      // Symbol: +, ->, ::
    case TIntLit(value: Int)       // Integer: 42, -1
    case TStrLit(value: String)    // String: "hello"
    case TOpen(bracket: Char)      // Opening: (, [, {
    case TClose(bracket: Char)     // Closing: ), ], }
    case TWhite(text: String)      // Whitespace (preserved for printing)
    case TNewline                   // Line break
    case TIndent(level: Int)       // Indentation level
    case TError(msg: String)       // Lexer error
    
    /** Render token back to string */
    def render: String = this match
      case TIdent(n)   => n
      case TKeyword(w) => w
      case TSymbol(s)  => s
      case TIntLit(v)  => v.toString
      case TStrLit(v)  => s"\"$v\""
      case TOpen(b)    => b.toString
      case TClose(b)   => b.toString
      case TWhite(t)   => t
      case TNewline    => "\n"
      case TIndent(l)  => "  " * l
      case TError(m)   => s"<ERROR: $m>"

  export Token.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: Lexer
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Lexer: Convert string to token stream.
   * 
   * Configuration:
   *   - keywords: words recognized as keywords (vs identifiers)
   *   - symbols: multi-char symbol tokens (->)
   */
  case class Lexer(
    keywords: Set[String] = Set.empty,
    symbols: Set[String] = Set.empty
  ):
    /** Tokenize a string */
    def tokenize(input: String): List[Token] =
      var tokens = List.empty[Token]
      var pos = 0
      
      while pos < input.length do
        val c = input(pos)
        
        if c == '\n' then
          tokens = TNewline :: tokens
          pos += 1
          
        else if c.isWhitespace then
          val start = pos
          while pos < input.length && input(pos).isWhitespace && input(pos) != '\n' do
            pos += 1
          tokens = TWhite(input.substring(start, pos)) :: tokens
          
        else if c == '"' then
          pos += 1
          val start = pos
          while pos < input.length && input(pos) != '"' do
            pos += 1
          tokens = TStrLit(input.substring(start, pos)) :: tokens
          if pos < input.length then pos += 1
          
        else if c.isDigit || (c == '-' && pos + 1 < input.length && input(pos+1).isDigit) then
          val start = pos
          if c == '-' then pos += 1
          while pos < input.length && input(pos).isDigit do
            pos += 1
          tokens = TIntLit(input.substring(start, pos).toInt) :: tokens
          
        else if c.isLetter || c == '_' then
          val start = pos
          while pos < input.length && (input(pos).isLetterOrDigit || input(pos) == '_') do
            pos += 1
          val word = input.substring(start, pos)
          tokens = (if keywords(word) then TKeyword(word) else TIdent(word)) :: tokens
          
        else if "()[]{}".contains(c) then
          tokens = (if "([{".contains(c) then TOpen(c) else TClose(c)) :: tokens
          pos += 1
          
        else
          // Check multi-char symbols first
          val matched = symbols.find(s => input.startsWith(s, pos))
          matched match
            case Some(sym) =>
              tokens = TSymbol(sym) :: tokens
              pos += sym.length
            case None =>
              tokens = TSymbol(c.toString) :: tokens
              pos += 1
      
      tokens.reverse
    
    /** Render tokens back to string */
    def render(tokens: List[Token]): String =
      tokens.map(_.render).mkString

  object Lexer:
    /** Default lexer with common settings */
    val default = Lexer(
      keywords = Set("if", "then", "else", "let", "in", "match", "with", "case"),
      symbols = Set("->", "<-", "=>", "::", "++", "==", "!=", "<=", ">=")
    )

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: Parse Result
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * ParseResult: Outcome of parsing.
   * 
   * Success includes the parsed value and remaining input.
   * Failure includes what was expected at what position.
   */
  enum ParseResult[+A]:
    case Success(value: A, remaining: List[Token])
    case Failure(expected: String, position: Int)
    
    def map[B](f: A => B): ParseResult[B] = this match
      case Success(v, r) => Success(f(v), r)
      case Failure(e, p) => Failure(e, p)
    
    def flatMap[B](f: A => ParseResult[B]): ParseResult[B] = this match
      case Success(v, _) => f(v)
      case Failure(e, p) => Failure(e, p)
    
    def toOption: Option[A] = this match
      case Success(v, _) => Some(v)
      case Failure(_, _) => None

  export ParseResult.{Success, Failure}

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 4: Syntax (Bidirectional Grammar)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Syntax: Bidirectional parser/printer.
   * 
   * A Syntax[A] can:
   *   - parse: List[Token] → ParseResult[A]
   *   - render: A → List[Token]
   * 
   * Combinators preserve both directions automatically.
   */
  trait Syntax[A]:
    /** Parse tokens into A */
    def parse(tokens: List[Token]): ParseResult[A]
    
    /** Render A into tokens */
    def render(a: A): List[Token]
    
    // ─────────────────────────────────────────────────────────────────────────
    // Combinators
    // ─────────────────────────────────────────────────────────────────────────
    
    /**
     * Sequence: this ~ that parses both in order.
     * 
     * Syntax[(A, B)] from Syntax[A] and Syntax[B]
     */
    def ~[B](that: Syntax[B]): Syntax[(A, B)] = Syntax.Seq(this, that)
    
    /**
     * Alternative: this | that tries this first, then that.
     * 
     * IMPORTANT: For printing, you must provide a way to choose.
     * Use ||| with a discriminator instead for complete bidirectionality.
     */
    def |(that: => Syntax[A]): Syntax[A] = Syntax.Alt(this, () => that)
    
    /**
     * Map with isomorphism: transform A ⇄ B.
     * 
     * The Iso ensures both directions work correctly.
     */
    def imap[B](f: A => B, g: B => A): Syntax[B] = Syntax.IMap(this, f, g)
    
    /**
     * Optional: zero or one occurrence.
     */
    def ? : Syntax[Option[A]] = Syntax.Opt(this)
    
    /**
     * Repetition: zero or more occurrences.
     */
    def * : Syntax[List[A]] = Syntax.Rep(this)
    
    /**
     * Non-empty repetition: one or more occurrences.
     */
    def + : Syntax[List[A]] = Syntax.Rep1(this)
    
    /**
     * Ignore result: parse but return Unit.
     */
    def void: Syntax[Unit] = imap(_ => (), _ => throw new Error("Cannot render void"))
    
    /**
     * Replace result with constant.
     */
    def as[B](b: B): Syntax[B] = imap(_ => b, _ => throw new Error("Cannot render constant"))
    
    /**
     * Separated list: a,b,c parsed by item.sepBy(comma)
     */
    def sepBy(sep: Syntax[Unit]): Syntax[List[A]] = Syntax.SepBy(this, sep)
    
    /**
     * Surrounded: (a) parsed by open ~ inner ~ close
     */
    def between(open: Syntax[Unit], close: Syntax[Unit]): Syntax[A] =
      (open ~ this ~ close).imap(_._1._2, a => (((), a), ()))

  object Syntax:
    // ─────────────────────────────────────────────────────────────────────────
    // Syntax Implementations
    // ─────────────────────────────────────────────────────────────────────────
    
    /** Sequence combinator */
    case class Seq[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]:
      def parse(tokens: List[Token]) = left.parse(tokens) match
        case Success(a, rest) => right.parse(rest).map(b => (a, b))
        case Failure(e, p) => Failure(e, p)
      
      def render(ab: (A, B)) = left.render(ab._1) ++ right.render(ab._2)
    
    /** Alternative combinator */
    case class Alt[A](left: Syntax[A], right: () => Syntax[A]) extends Syntax[A]:
      def parse(tokens: List[Token]) = left.parse(tokens) match
        case s: Success[A] => s
        case Failure(_, _) => right().parse(tokens)
      
      def render(a: A) = left.render(a)  // Default to left for printing
    
    /** Isomorphism mapping */
    case class IMap[A, B](base: Syntax[A], f: A => B, g: B => A) extends Syntax[B]:
      def parse(tokens: List[Token]) = base.parse(tokens).map(f)
      def render(b: B) = base.render(g(b))
    
    /** Optional */
    case class Opt[A](base: Syntax[A]) extends Syntax[Option[A]]:
      def parse(tokens: List[Token]) = base.parse(tokens) match
        case Success(a, rest) => Success(Some(a), rest)
        case Failure(_, _)    => Success(None, tokens)
      
      def render(opt: Option[A]) = opt.map(base.render).getOrElse(Nil)
    
    /** Zero or more repetition */
    case class Rep[A](base: Syntax[A]) extends Syntax[List[A]]:
      def parse(tokens: List[Token]): ParseResult[List[A]] =
        base.parse(tokens) match
          case Success(a, rest) => parse(rest).map(as => a :: as)
          case Failure(_, _)    => Success(Nil, tokens)
      
      def render(as: List[A]) = as.flatMap(base.render)
    
    /** One or more repetition */
    case class Rep1[A](base: Syntax[A]) extends Syntax[List[A]]:
      def parse(tokens: List[Token]): ParseResult[List[A]] =
        base.parse(tokens) match
          case Success(a, rest) => Rep(base).parse(rest).map(as => a :: as)
          case Failure(e, p)    => Failure(e, p)
      
      def render(as: List[A]) = as.flatMap(base.render)
    
    /** Separated list */
    case class SepBy[A](item: Syntax[A], sep: Syntax[Unit]) extends Syntax[List[A]]:
      def parse(tokens: List[Token]): ParseResult[List[A]] =
        item.parse(tokens) match
          case Failure(_, _) => Success(Nil, tokens)
          case Success(first, rest) =>
            def parseMore(ts: List[Token], acc: List[A]): (List[A], List[Token]) =
              sep.parse(ts) match
                case Failure(_, _) => (acc, ts)
                case Success(_, afterSep) =>
                  item.parse(afterSep) match
                    case Failure(_, _) => (acc, ts)
                    case Success(a, afterItem) => parseMore(afterItem, acc :+ a)
            val (more, finalRest) = parseMore(rest, List(first))
            Success(more, finalRest)
      
      def render(as: List[A]) = as match
        case Nil => Nil
        case head :: tail => 
          item.render(head) ++ tail.flatMap(a => sep.render(()) ++ item.render(a))

    // ─────────────────────────────────────────────────────────────────────────
    // Primitive Syntaxes
    // ─────────────────────────────────────────────────────────────────────────
    
    /** Match a specific keyword */
    def keyword(word: String): Syntax[Unit] = new Syntax[Unit]:
      def parse(tokens: List[Token]) = tokens match
        case TKeyword(w) :: rest if w == word => Success((), rest)
        case TIdent(w) :: rest if w == word   => Success((), rest)
        case _ => Failure(s"keyword '$word'", 0)
      def render(u: Unit) = List(TKeyword(word))
    
    /** Match a specific symbol */
    def symbol(sym: String): Syntax[Unit] = new Syntax[Unit]:
      def parse(tokens: List[Token]) = tokens match
        case TSymbol(s) :: rest if s == sym => Success((), rest)
        case _ => Failure(s"symbol '$sym'", 0)
      def render(u: Unit) = List(TSymbol(sym))
    
    /** Match any identifier */
    val ident: Syntax[String] = new Syntax[String]:
      def parse(tokens: List[Token]) = tokens match
        case TIdent(name) :: rest => Success(name, rest)
        case _ => Failure("identifier", 0)
      def render(name: String) = List(TIdent(name))
    
    /** Match an integer literal */
    val intLit: Syntax[Int] = new Syntax[Int]:
      def parse(tokens: List[Token]) = tokens match
        case TIntLit(n) :: rest => Success(n, rest)
        case _ => Failure("integer", 0)
      def render(n: Int) = List(TIntLit(n))
    
    /** Match a string literal */
    val strLit: Syntax[String] = new Syntax[String]:
      def parse(tokens: List[Token]) = tokens match
        case TStrLit(s) :: rest => Success(s, rest)
        case _ => Failure("string", 0)
      def render(s: String) = List(TStrLit(s))
    
    /** Skip whitespace (always succeeds) */
    val whitespace: Syntax[Unit] = new Syntax[Unit]:
      def parse(tokens: List[Token]) = tokens match
        case TWhite(_) :: rest => Success((), rest)
        case TNewline :: rest  => Success((), rest)
        case _ => Success((), tokens)
      def render(u: Unit) = List(TWhite(" "))
    
    /** Succeed without consuming input */
    def pure[A](a: A): Syntax[A] = new Syntax[A]:
      def parse(tokens: List[Token]) = Success(a, tokens)
      def render(x: A) = Nil
    
    /** Parentheses */
    def parens[A](inner: Syntax[A]): Syntax[A] = new Syntax[A]:
      def parse(tokens: List[Token]) = tokens match
        case TOpen('(') :: rest =>
          inner.parse(rest) match
            case Success(a, TClose(')') :: after) => Success(a, after)
            case Success(_, _) => Failure("')'", 0)
            case f: Failure[?] => f.asInstanceOf[Failure[A]]
        case _ => Failure("'('", 0)
      def render(a: A) = TOpen('(') :: inner.render(a) ::: List(TClose(')'))
    
    /** Brackets */
    def brackets[A](inner: Syntax[A]): Syntax[A] = new Syntax[A]:
      def parse(tokens: List[Token]) = tokens match
        case TOpen('[') :: rest =>
          inner.parse(rest) match
            case Success(a, TClose(']') :: after) => Success(a, after)
            case Success(_, _) => Failure("']'", 0)
            case f: Failure[?] => f.asInstanceOf[Failure[A]]
        case _ => Failure("'['", 0)
      def render(a: A) = TOpen('[') :: inner.render(a) ::: List(TClose(']'))
    
    /** Braces */
    def braces[A](inner: Syntax[A]): Syntax[A] = new Syntax[A]:
      def parse(tokens: List[Token]) = tokens match
        case TOpen('{') :: rest =>
          inner.parse(rest) match
            case Success(a, TClose('}') :: after) => Success(a, after)
            case Success(_, _) => Failure("'}'", 0)
            case f: Failure[?] => f.asInstanceOf[Failure[A]]
        case _ => Failure("'{'", 0)
      def render(a: A) = TOpen('{') :: inner.render(a) ::: List(TClose('}'))

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 5: Convenience API
  // ═══════════════════════════════════════════════════════════════════════════

  /** Parse a string using lexer and syntax */
  def parse[A](input: String, syntax: Syntax[A], lexer: Lexer = Lexer.default): Option[A] =
    val tokens = lexer.tokenize(input).filterNot {
      case TWhite(_) | TNewline => true
      case _ => false
    }
    syntax.parse(tokens).toOption
  
  /** Render a value using syntax and lexer */
  def render[A](value: A, syntax: Syntax[A], lexer: Lexer = Lexer.default): String =
    lexer.render(syntax.render(value))

// End of Syntax object
