package phi

import Val.*

/**
 * Minimal Syntax infrastructure for bidirectional parsing/printing
 */

// Token stream for parsing
enum Lex:
  case Ident(name: String)
  case Keyword(kw: String)
  case Symbol(sym: String)
  case IntLit(n: Int)
  case StrLit(s: String)
  case Whitespace(ws: String)
  case EOF
  case HoleTok(label: Option[String])

  def render: String = this match
    case Ident(n) => n
    case Keyword(k) => k
    case Symbol(s) => s
    case IntLit(n) => n.toString
    case StrLit(s) => s"\"$s\""
    case Whitespace(ws) => ws
    case EOF => ""
    case HoleTok(_) => "?"

// Simple lexer
object Lexer:
  def tokenize(input: String): TokenStream =
    val tokens = scala.collection.mutable.ListBuffer[Lex]()
    var i = 0
    while i < input.length do
      val c = input(i)
      if c.isWhitespace then
        val start = i
        while i < input.length && input(i).isWhitespace do i += 1
        tokens += Lex.Whitespace(input.substring(start, i))
      else if c.isLetter || c == '_' then
        val start = i
        while i < input.length && (input(i).isLetterOrDigit || input(i) == '_') do i += 1
        val word = input.substring(start, i)
        tokens += Lex.Ident(word)
      else if c.isDigit then
        val start = i
        while i < input.length && input(i).isDigit do i += 1
        tokens += Lex.IntLit(input.substring(start, i).toInt)
      else if c == '"' then
        i += 1
        val start = i
        while i < input.length && input(i) != '"' do i += 1
        tokens += Lex.StrLit(input.substring(start, i))
        if i < input.length then i += 1
      else
        tokens += Lex.Symbol(c.toString)
        i += 1
    tokens += Lex.EOF
    TokenStream(tokens.toVector, 0)

case class TokenStream(tokens: Vector[Lex], pos: Int):
  def peek: Lex = if pos < tokens.length then tokens(pos) else Lex.EOF
  def advance: TokenStream = copy(pos = pos + 1)
  def skipWs: TokenStream =
    var p = pos
    while p < tokens.length && tokens(p).isInstanceOf[Lex.Whitespace] do p += 1
    copy(pos = p)
  def nonEmpty: Boolean = pos < tokens.length && peek != Lex.EOF

// Parse result with partial terms
enum Term[+A]:
  case Done(value: A)
  case Hole(label: Option[String])
  def isDone: Boolean = this.isInstanceOf[Done[?]]

case class ParseResult[A](term: Term[A], remaining: TokenStream)
object ParseResult:
  def done[A](a: A, s: TokenStream): ParseResult[A] = ParseResult(Term.Done(a), s)
  def hole[A](s: TokenStream, label: Option[String] = None): ParseResult[A] = ParseResult(Term.Hole(label), s)

// Bidirectional syntax
trait Syntax[A]:
  def parse(stream: TokenStream): ParseResult[A]
  def render(term: Term[A]): Vector[Lex]
  
  def ~[B](that: Syntax[B]): Syntax[(A, B)] = Syntax.Seq(this, that)
  def |[B >: A](that: Syntax[B]): Syntax[B] = Syntax.Choice(this.asInstanceOf[Syntax[B]], that)
  def iso[B](iso: Iso[A, B]): Syntax[B] = Syntax.Mapped(this, iso)

case class Iso[A, B](forward: A => B, backward: B => A)

object Syntax:
  class RenderMismatch extends RuntimeException("render mismatch")
  
  def pure[A](a: A): Syntax[A] = new Syntax[A]:
    def parse(s: TokenStream) = ParseResult.done(a, s)
    def render(t: Term[A]) = Vector.empty
  
  def fail[A](msg: String): Syntax[A] = new Syntax[A]:
    def parse(s: TokenStream) = ParseResult.hole(s, Some(msg))
    def render(t: Term[A]) = Vector.empty

  case class Seq[A, B](first: Syntax[A], second: Syntax[B]) extends Syntax[(A, B)]:
    def parse(stream: TokenStream): ParseResult[(A, B)] =
      val r1 = first.parse(stream)
      r1.term match
        case Term.Done(a) =>
          val r2 = second.parse(r1.remaining)
          r2.term match
            case Term.Done(b) => ParseResult.done((a, b), r2.remaining)
            case Term.Hole(l) => ParseResult.hole(r2.remaining, l)
        case Term.Hole(l) => ParseResult.hole(r1.remaining, l)
    def render(term: Term[(A, B)]): Vector[Lex] = term match
      case Term.Done((a, b)) => first.render(Term.Done(a)) ++ second.render(Term.Done(b))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))

  case class Choice[A](first: Syntax[A], second: Syntax[A]) extends Syntax[A]:
    def parse(stream: TokenStream): ParseResult[A] =
      val r1 = first.parse(stream)
      if r1.term.isDone then r1 else second.parse(stream)
    def render(term: Term[A]): Vector[Lex] =
      try first.render(term)
      catch case _: RenderMismatch => second.render(term)

  case class Mapped[A, B](inner: Syntax[A], iso: Iso[A, B]) extends Syntax[B]:
    def parse(stream: TokenStream): ParseResult[B] =
      val r = inner.parse(stream)
      r.term match
        case Term.Done(a) => ParseResult.done(iso.forward(a), r.remaining)
        case Term.Hole(l) => ParseResult.hole(r.remaining, l)
    def render(term: Term[B]): Vector[Lex] = term match
      case Term.Done(b) => inner.render(Term.Done(iso.backward(b)))
      case Term.Hole(l) => Vector(Lex.HoleTok(l))

object Renderer:
  def render(tokens: Vector[Lex]): String = tokens.map(_.render).mkString
  def render[A](syntax: Syntax[A], term: Term[A]): String = render(syntax.render(term))
