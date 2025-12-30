package phi.meta

/**
 * Minimal Syntax infrastructure for bidirectional parsing/printing
 */

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
