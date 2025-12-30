package phi

/**
 * Lexical tokens for the Phi language.
 */
enum Lex:
  case Ident(name: String)
  case IntLit(value: Int)
  case StringLit(value: String)
  case Symbol(sym: String)
  case Keyword(kw: String)
  case Whitespace(ws: String)
  case Newline
  case HoleTok(label: Option[String])
  case EOF

  def render: String = this match
    case Ident(n)      => n
    case IntLit(v)     => v.toString
    case StringLit(v)  => s"\"$v\""
    case Symbol(s)     => s
    case Keyword(k)    => k
    case Whitespace(w) => w
    case Newline       => "\n"
    case HoleTok(l)    => l.map(n => s"?$n").getOrElse("?")
    case EOF           => ""

object Lex:
  val keywords: Set[String] = Set(
    "let", "in", "if", "then", "else", "case", "of", "fn", "fun",
    "type", "data", "where", "import", "module", "grammar", "rule"
  )

  val symbols: Set[String] = Set(
    "(", ")", "[", "]", "{", "}", ",", ";", ":", "::", ".", "..",
    "=", "==", "!=", "<", ">", "<=", ">=", "+", "-", "*", "/", "%",
    "&&", "||", "!", "->", "<-", "=>", "|", "&", "^", "~", "@", "#"
  )

/**
 * Token stream for parsing.
 */
case class TokenStream(tokens: Vector[Lex], pos: Int = 0):
  def current: Option[Lex] =
    if pos < tokens.length then Some(tokens(pos)) else None

  def peek: Lex = current.getOrElse(Lex.EOF)

  def advance: TokenStream = copy(pos = pos + 1)

  def advance(n: Int): TokenStream = copy(pos = pos + n)

  def isEmpty: Boolean = pos >= tokens.length

  def nonEmpty: Boolean = !isEmpty

  def remaining: Vector[Lex] = tokens.drop(pos)

  def ++(other: TokenStream): TokenStream =
    TokenStream(tokens ++ other.tokens, pos)

  /** Skip whitespace and newlines */
  def skipWs: TokenStream =
    var stream = this
    while stream.current.exists {
      case Lex.Whitespace(_) | Lex.Newline => true
      case _ => false
    } do stream = stream.advance
    stream

object TokenStream:
  def apply(tokens: Lex*): TokenStream = TokenStream(tokens.toVector)
  def empty: TokenStream = TokenStream(Vector.empty)

/**
 * Simple lexer for tokenizing input strings.
 */
object Lexer:
  def tokenize(input: String): TokenStream =
    val tokens = Vector.newBuilder[Lex]
    var i = 0

    while i < input.length do
      val c = input(i)
      
      // Whitespace
      if c == ' ' || c == '\t' then
        val start = i
        while i < input.length && (input(i) == ' ' || input(i) == '\t') do i += 1
        tokens += Lex.Whitespace(input.substring(start, i))
      
      // Newline
      else if c == '\n' then
        tokens += Lex.Newline
        i += 1
      else if c == '\r' then
        i += 1
        if i < input.length && input(i) == '\n' then i += 1
        tokens += Lex.Newline
      
      // Hole
      else if c == '?' then
        i += 1
        if i < input.length && input(i).isLetter then
          val start = i
          while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
          tokens += Lex.HoleTok(Some(input.substring(start, i)))
        else
          tokens += Lex.HoleTok(None)
      
      // String literal
      else if c == '"' then
        i += 1
        val sb = StringBuilder()
        while i < input.length && input(i) != '"' do
          if input(i) == '\\' && i + 1 < input.length then
            i += 1
            input(i) match
              case 'n'  => sb += '\n'
              case 't'  => sb += '\t'
              case '\\' => sb += '\\'
              case '"'  => sb += '"'
              case _    => sb += input(i)
          else
            sb += input(i)
          i += 1
        if i < input.length then i += 1 // skip closing quote
        tokens += Lex.StringLit(sb.toString)
      
      // Number
      else if c.isDigit then
        val start = i
        while i < input.length && input(i).isDigit do i += 1
        tokens += Lex.IntLit(input.substring(start, i).toInt)
      
      // Identifier or keyword
      else if c.isLetter || c == '_' then
        val start = i
        while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
        val word = input.substring(start, i)
        if Lex.keywords.contains(word) then
          tokens += Lex.Keyword(word)
        else
          tokens += Lex.Ident(word)
      
      // Multi-char symbols
      else if i + 1 < input.length then
        val two = input.substring(i, i + 2)
        if Lex.symbols.contains(two) then
          tokens += Lex.Symbol(two)
          i += 2
        else if Lex.symbols.contains(c.toString) then
          tokens += Lex.Symbol(c.toString)
          i += 1
        else
          tokens += Lex.Symbol(c.toString)
          i += 1
      
      // Single-char symbol
      else
        tokens += Lex.Symbol(c.toString)
        i += 1

    TokenStream(tokens.result())

/**
 * Parse result containing the parsed term and remaining stream.
 */
case class ParseResult[+A](term: Term[A], remaining: TokenStream):
  def map[B](f: A => B): ParseResult[B] = ParseResult(term.map(f), remaining)

  def flatMap[B](f: A => ParseResult[B]): ParseResult[B] =
    term match
      case Term.Done(a) => f(a)
      case Term.Hole(l) => ParseResult(Term.Hole(l), remaining)

object ParseResult:
  def done[A](value: A, remaining: TokenStream): ParseResult[A] =
    ParseResult(Term.Done(value), remaining)

  def hole[A](remaining: TokenStream, label: Option[String] = None): ParseResult[A] =
    ParseResult(Term.Hole(label), remaining)

/**
 * Isomorphism for bidirectional transformation.
 */
trait Iso[A, B]:
  def to(a: A): B
  def from(b: B): A

  def inverse: Iso[B, A] = Iso(from, to)

  def andThen[C](other: Iso[B, C]): Iso[A, C] =
    Iso(a => other.to(to(a)), c => from(other.from(c)))

object Iso:
  def apply[A, B](f: A => B, g: B => A): Iso[A, B] = new Iso[A, B]:
    def to(a: A): B = f(a)
    def from(b: B): A = g(b)

  def id[A]: Iso[A, A] = Iso(identity, identity)

/**
 * Syntax combinator for parsing with round-trip rendering support.
 */
trait Syntax[A]:
  /** Parse from token stream, producing a Term */
  def parse(stream: TokenStream): ParseResult[A]

  /** Render a term back to tokens */
  def render(term: Term[A]): Vector[Lex]

  /** Map with isomorphism for round-trip correctness */
  def iso[B](iso: Iso[A, B]): Syntax[B] = Syntax.IsoSyntax(this, iso)

  /** Sequence: parse this then that, keep both */
  def ~[B](that: Syntax[B]): Syntax[(A, B)] = Syntax.Seq(this, that)

  /** Sequence: parse this then that, keep right */
  def *>[B](that: Syntax[B]): Syntax[B] = (this ~ that).iso(Iso[(A, B), B](p => p._2, b => (().asInstanceOf[A], b)))

  /** Sequence: parse this then that, keep left */
  def <*[B](that: Syntax[B]): Syntax[A] = (this ~ that).iso(Iso[(A, B), A](p => p._1, a => (a, ().asInstanceOf[B])))

  /** Choice: try this, if fails try that */
  def |[B >: A](that: Syntax[B]): Syntax[B] = Syntax.Choice(this.asInstanceOf[Syntax[B]], that)

  /** Optional: parse zero or one */
  def opt: Syntax[Option[A]] = Syntax.Opt(this)

  /** Many: parse zero or more */
  def many: Syntax[List[A]] = Syntax.Many(this)

  /** Many1: parse one or more */
  def many1: Syntax[List[A]] = (this ~ this.many).iso(Iso(
    { case (h, t) => h :: t },
    { case h :: t => (h, t); case Nil => throw new Exception("many1 requires at least one") }
  ))

  /** Separated by */
  def sepBy[S](sep: Syntax[S]): Syntax[List[A]] =
    (this ~ (sep *> this).many).iso(Iso(
      { case (h, t) => h :: t },
      { case h :: t => (h, t); case Nil => throw new Exception("sepBy requires at least one") }
    )).opt.iso(Iso(_.getOrElse(Nil), l => if l.isEmpty then None else Some(l)))

object Syntax:
  /** Parse a specific keyword */
  def keyword(kw: String): Syntax[Unit] = new Syntax[Unit]:
    def parse(stream: TokenStream): ParseResult[Unit] =
      stream.skipWs.peek match
        case Lex.Keyword(k) if k == kw => ParseResult.done((), stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some(s"expected '$kw'"))

    def render(term: Term[Unit]): Vector[Lex] = Vector(Lex.Keyword(kw))

  /** Parse a specific symbol */
  def symbol(sym: String): Syntax[Unit] = new Syntax[Unit]:
    def parse(stream: TokenStream): ParseResult[Unit] =
      stream.skipWs.peek match
        case Lex.Symbol(s) if s == sym => ParseResult.done((), stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some(s"expected '$sym'"))

    def render(term: Term[Unit]): Vector[Lex] = Vector(Lex.Symbol(sym))

  /** Parse any identifier */
  def ident: Syntax[String] = new Syntax[String]:
    def parse(stream: TokenStream): ParseResult[String] =
      stream.skipWs.peek match
        case Lex.Ident(name) => ParseResult.done(name, stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some("expected identifier"))

    def render(term: Term[String]): Vector[Lex] = term match
      case Term.Done(name) => Vector(Lex.Ident(name))
      case Term.Hole(l)    => Vector(Lex.HoleTok(l))

  /** Parse any integer literal */
  def intLit: Syntax[Int] = new Syntax[Int]:
    def parse(stream: TokenStream): ParseResult[Int] =
      stream.skipWs.peek match
        case Lex.IntLit(v) => ParseResult.done(v, stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some("expected integer"))

    def render(term: Term[Int]): Vector[Lex] = term match
      case Term.Done(v) => Vector(Lex.IntLit(v))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))

  /** Parse any string literal */
  def stringLit: Syntax[String] = new Syntax[String]:
    def parse(stream: TokenStream): ParseResult[String] =
      stream.skipWs.peek match
        case Lex.StringLit(v) => ParseResult.done(v, stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some("expected string"))

    def render(term: Term[String]): Vector[Lex] = term match
      case Term.Done(v) => Vector(Lex.StringLit(v))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))

  /** Parse a hole marker */
  def hole[A]: Syntax[Term[A]] = new Syntax[Term[A]]:
    def parse(stream: TokenStream): ParseResult[Term[A]] =
      stream.skipWs.peek match
        case Lex.HoleTok(label) => ParseResult.done(Term.Hole(label), stream.skipWs.advance)
        case _ => ParseResult.hole(stream, Some("expected hole"))

    def render(term: Term[Term[A]]): Vector[Lex] = term match
      case Term.Done(Term.Hole(l)) => Vector(Lex.HoleTok(l))
      case Term.Done(Term.Done(_)) => Vector() // nested done - shouldn't render as hole
      case Term.Hole(l)            => Vector(Lex.HoleTok(l))

  /** Pure value (always succeeds, consumes nothing) */
  def pure[A](value: A): Syntax[A] = new Syntax[A]:
    def parse(stream: TokenStream): ParseResult[A] = ParseResult.done(value, stream)
    def render(term: Term[A]): Vector[Lex] = Vector.empty

  /** Fail with a hole */
  def fail[A](label: String): Syntax[A] = new Syntax[A]:
    def parse(stream: TokenStream): ParseResult[A] = ParseResult.hole(stream, Some(label))
    def render(term: Term[A]): Vector[Lex] = term match
      case Term.Hole(l) => Vector(Lex.HoleTok(l))
      case _            => Vector.empty

  /** Lazy syntax for recursive definitions */
  def delay[A](syntax: => Syntax[A]): Syntax[A] = new Syntax[A]:
    lazy val inner = syntax
    def parse(stream: TokenStream): ParseResult[A] = inner.parse(stream)
    def render(term: Term[A]): Vector[Lex] = inner.render(term)

  // Internal implementations
  private case class IsoSyntax[A, B](base: Syntax[A], iso: Iso[A, B]) extends Syntax[B]:
    def parse(stream: TokenStream): ParseResult[B] = base.parse(stream).map(iso.to)
    def render(term: Term[B]): Vector[Lex] = base.render(term.map(iso.from))

  private case class Seq[A, B](left: Syntax[A], right: Syntax[B]) extends Syntax[(A, B)]:
    def parse(stream: TokenStream): ParseResult[(A, B)] =
      val r1 = left.parse(stream)
      r1.term match
        case Term.Done(a) =>
          val r2 = right.parse(r1.remaining)
          r2.term match
            case Term.Done(b) => ParseResult.done((a, b), r2.remaining)
            case Term.Hole(l) => ParseResult.hole(r2.remaining, l)
        case Term.Hole(l) => ParseResult.hole(r1.remaining, l)

    def render(term: Term[(A, B)]): Vector[Lex] = term match
      case Term.Done((a, b)) => left.render(Term.Done(a)) ++ right.render(Term.Done(b))
      case Term.Hole(l)      => Vector(Lex.HoleTok(l))

  /** Exception thrown when an Iso's reverse function doesn't match the value */
  class RenderMismatch extends RuntimeException("render mismatch")
  
  private case class Choice[A](first: Syntax[A], second: Syntax[A]) extends Syntax[A]:
    def parse(stream: TokenStream): ParseResult[A] =
      val r1 = first.parse(stream)
      if r1.term.isDone then r1
      else
        val r2 = second.parse(stream)
        if r2.term.isDone then r2
        else r1 // Return first failure

    def render(term: Term[A]): Vector[Lex] =
      // Try first syntax for rendering, if it throws RenderMismatch, try second
      try first.render(term)
      catch case _: RenderMismatch => second.render(term)

  private case class Opt[A](inner: Syntax[A]) extends Syntax[Option[A]]:
    def parse(stream: TokenStream): ParseResult[Option[A]] =
      val r = inner.parse(stream)
      r.term match
        case Term.Done(a) => ParseResult.done(Some(a), r.remaining)
        case Term.Hole(_) => ParseResult.done(None, stream)

    def render(term: Term[Option[A]]): Vector[Lex] = term match
      case Term.Done(Some(a)) => inner.render(Term.Done(a))
      case Term.Done(None)    => Vector.empty
      case Term.Hole(l)       => Vector(Lex.HoleTok(l))

  private case class Many[A](inner: Syntax[A]) extends Syntax[List[A]]:
    def parse(stream: TokenStream): ParseResult[List[A]] =
      val results = List.newBuilder[A]
      var current = stream
      var continue = true
      while continue do
        val r = inner.parse(current)
        r.term match
          case Term.Done(a) =>
            results += a
            current = r.remaining
          case Term.Hole(_) =>
            continue = false
      ParseResult.done(results.result(), current)

    def render(term: Term[List[A]]): Vector[Lex] = term match
      case Term.Done(list) => list.flatMap(a => inner.render(Term.Done(a))).toVector
      case Term.Hole(l)    => Vector(Lex.HoleTok(l))

/**
 * Helper to render tokens back to string.
 */
object Renderer:
  def render(tokens: Vector[Lex]): String =
    tokens.map(_.render).mkString

  def render[A](syntax: Syntax[A], term: Term[A]): String =
    render(syntax.render(term))
