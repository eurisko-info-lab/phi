package phi.core

import phi.meta.{Val, Term}
import Val.*, Term.*

/**
 * CORE: Maximum density algebraic foundation.
 * 
 * Everything derives from three structures:
 *   Fix[F]      = μF          (recursive data)
 *   Free[F,A]   = A + F[Free] (effects)  
 *   Cofree[F,A] = A × F[Cofree] (annotated trees)
 * 
 * Plus three accessors:
 *   Lens[S,A]   = (S→A, A→S→S)    (exactly one)
 *   Prism[S,A]  = (S→?A, A→S)     (maybe one)
 *   ∀[S,A]      = List[A] × (f→S) (zero or more)
 */

// ═══════════════════════════════════════════════════════════════════════════
// FOUNDATION: Three typeclasses, everything else derived
// ═══════════════════════════════════════════════════════════════════════════

trait F[X[_]] { def map[A,B](xa: X[A])(f: A => B): X[B] }
trait M[X[_]] extends F[X] { def pure[A](a: A): X[A]; def bind[A,B](xa: X[A])(f: A => X[B]): X[B]; def map[A,B](xa: X[A])(f: A => B) = bind(xa)(a => pure(f(a))) }
trait W[X[_]] extends F[X] { def extract[A](xa: X[A]): A; def extend[A,B](xa: X[A])(f: X[A] => B): X[B] }
trait ~>[X[_], Y[_]] { def apply[A](xa: X[A]): Y[A] }

// ═══════════════════════════════════════════════════════════════════════════
// PATTERN FUNCTOR: ValF - one layer of Val
// ═══════════════════════════════════════════════════════════════════════════

enum V[+A]: // V for "Val pattern functor" - single character
  case C(n: String, a: List[A])  // Constructor
  case S(s: String)               // String
  case I(i: Int)                  // Int
  case L(l: List[A])              // List

object V:
  given F[V] with
    def map[A,B](v: V[A])(f: A => B): V[B] = v match
      case C(n,a) => C(n, a.map(f)); case S(s) => S(s); case I(i) => I(i); case L(l) => L(l.map(f))
  
  def out(v: Val): V[Val] = v match
    case VCon(n,a) => C(n,a); case VStr(s) => S(s); case VInt(i) => I(i); case VList(l) => L(l)
  
  def in(v: V[Val]): Val = v match
    case C(n,a) => VCon(n,a); case S(s) => VStr(s); case I(i) => VInt(i); case L(l) => VList(l)

// ═══════════════════════════════════════════════════════════════════════════
// FIX + RECURSION SCHEMES: μF with fold/unfold
// ═══════════════════════════════════════════════════════════════════════════

case class μ[X[_]](out: X[μ[X]])  // Fix point

object μ:
  def cata[X[_]: F, A](f: X[A] => A)(t: μ[X]): A = f(summon[F[X]].map(t.out)(cata(f)))
  def ana[X[_]: F, A](f: A => X[A])(a: A): μ[X] = μ(summon[F[X]].map(f(a))(ana(f)))
  def hylo[X[_]: F, A, B](f: X[B] => B)(g: A => X[A])(a: A): B = f(summon[F[X]].map(g(a))(hylo(f)(g)))
  def para[X[_]: F, A](f: X[(μ[X], A)] => A)(t: μ[X]): A = f(summon[F[X]].map(t.out)(x => (x, para(f)(x))))
  
  // Val-specific schemes (since Val isn't Fix-based in meta.scala)
  def cataV[A](f: V[A] => A)(v: Val): A = f(summon[F[V]].map(V.out(v))(cataV(f)))
  def anaV[A](f: A => V[A])(a: A): Val = V.in(summon[F[V]].map(f(a))(anaV(f)))

// ═══════════════════════════════════════════════════════════════════════════
// FREE MONAD: A + F[Free[F,A]]
// ═══════════════════════════════════════════════════════════════════════════

enum Fr[X[_], A]:  // Free monad
  case Pu(a: A)                // Pure
  case Su(x: X[Fr[X, A]])      // Suspend

object Fr:
  def pure[X[_], A](a: A): Fr[X, A] = Pu(a)
  def lift[X[_]: F, A](xa: X[A]): Fr[X, A] = Su(summon[F[X]].map(xa)(Pu(_)))
  
  given [X[_]: F]: M[[A] =>> Fr[X, A]] with
    def pure[A](a: A) = Pu(a)
    def bind[A, B](m: Fr[X, A])(f: A => Fr[X, B]): Fr[X, B] = m match
      case Pu(a)  => f(a)
      case Su(xa) => Su(summon[F[X]].map(xa)(bind(_)(f)))
  
  def run[X[_]: F, Y[_]: M, A](p: Fr[X, A])(n: X ~> Y): Y[A] = p match
    case Pu(a)  => summon[M[Y]].pure(a)
    case Su(xa) => summon[M[Y]].bind(n(xa))(run(_)(n))

// ═══════════════════════════════════════════════════════════════════════════
// COFREE COMONAD: A × F[Cofree[F,A]]
// ═══════════════════════════════════════════════════════════════════════════

case class Co[X[_], A](h: A, t: X[Co[X, A]])  // Cofree

object Co:
  given [X[_]: F]: W[[A] =>> Co[X, A]] with
    def map[A, B](c: Co[X, A])(f: A => B): Co[X, B] = Co(f(c.h), summon[F[X]].map(c.t)(map(_)(f)))
    def extract[A](c: Co[X, A]): A = c.h
    def extend[A, B](c: Co[X, A])(f: Co[X, A] => B): Co[X, B] = Co(f(c), summon[F[X]].map(c.t)(extend(_)(f)))
  
  // Annotate Val with computed attributes
  def ann[A](v: Val)(f: Val => A): Co[V, A] = Co(f(v), V.out(v) match
    case V.C(n, a) => V.C(n, a.map(ann(_)(f)))
    case V.S(s)    => V.S(s)
    case V.I(i)    => V.I(i)
    case V.L(l)    => V.L(l.map(ann(_)(f))))
  
  // Forget annotations, recover Val
  def forget(c: Co[V, ?]): Val = V.in(summon[F[V]].map(c.t)(forget))

// ═══════════════════════════════════════════════════════════════════════════
// OPTICS: Lens, Prism, Traversal - unified as profunctor
// ═══════════════════════════════════════════════════════════════════════════

case class Ln[S, A](get: S => A, set: A => S => S):  // Lens
  def mod(f: A => A)(s: S): S = set(f(get(s)))(s)
  def >>[B](o: Ln[A, B]): Ln[S, B] = Ln(s => o.get(get(s)), b => s => set(o.set(b)(get(s)))(s))

case class Pr[S, A](get: S => Option[A], rev: A => S):  // Prism
  def mod(f: A => A)(s: S): S = get(s).map(a => rev(f(a))).getOrElse(s)

trait Tr[S, A]:  // Traversal
  def all(s: S): List[A]; def mod(f: A => A)(s: S): S

object Op:  // Optics for Val
  def arg(n: Int): Ln[Val, Val] = Ln(
    { case VCon(_, a) => a(n); case v => v },
    (x: Val) => { case VCon(m, a) => VCon(m, a.updated(n, x)); case s => s })
  
  def con(n: String): Pr[Val, List[Val]] = Pr(
    { case VCon(m, a) if m == n => Some(a); case _ => None },
    a => VCon(n, a))
  
  val ch: Tr[Val, Val] = new Tr[Val, Val]:
    def all(s: Val) = s match { case VCon(_, a) => a; case VList(l) => l; case _ => Nil }
    def mod(f: Val => Val)(s: Val) = s match
      case VCon(n, a) => VCon(n, a.map(f)); case VList(l) => VList(l.map(f)); case v => v
  
  def everywhere(f: Val => Val): Val => Val = v => f(ch.mod(everywhere(f))(v))

// ═══════════════════════════════════════════════════════════════════════════
// XFORM: Bidirectional transformation (Iso subset)
// ═══════════════════════════════════════════════════════════════════════════

trait X[A, B]:  // Xform
  def fwd(a: A): Option[B]; def bwd(b: B): Option[A]
  def >>>[C](o: X[B, C]): X[A, C] = X.comp(this, o)
  def inv: X[B, A] = X(bwd, fwd)

object X:
  def apply[A, B](f: A => Option[B], g: B => Option[A]): X[A, B] = new X[A, B] { def fwd(a: A) = f(a); def bwd(b: B) = g(b) }
  def id[A]: X[A, A] = X(Some(_), Some(_))
  def comp[A, B, C](f: X[A, B], g: X[B, C]): X[A, C] = X(a => f.fwd(a).flatMap(g.fwd), c => g.bwd(c).flatMap(f.bwd))

// ═══════════════════════════════════════════════════════════════════════════
// EDIT: Simple change algebra
// ═══════════════════════════════════════════════════════════════════════════

enum Ed[+A]:  // Edit
  case Ins[V](v: V) extends Ed[V]
  case Del extends Ed[Nothing]
  case Rep[V](v: V) extends Ed[V]
  case Seq[A, B](a: Ed[A], b: Ed[B]) extends Ed[B]

object Ed:
  def ins[A](a: A): Ed[A] = Ins(a)
  def del: Ed[Nothing] = Del
  def rep[A](a: A): Ed[A] = Rep(a)
  
  def apply[A](e: Ed[A], t: Term[A]): Term[A] = e match
    case Ins(v)    => t match { case Term.Hole(_) => Term.Done(v.asInstanceOf[A]); case _ => t }
    case Del       => Term.Hole(None)
    case Rep(v)    => Term.Done(v.asInstanceOf[A])
    case Seq(a, b) => apply(b, apply(a.asInstanceOf[Ed[A]], t))

// ═══════════════════════════════════════════════════════════════════════════
// VALIDATED: Parallel error accumulation
// ═══════════════════════════════════════════════════════════════════════════

enum Vd[+E, +A]:  // Validated
  case Ok(a: A)
  case No(e: List[E])
  def map[B](f: A => B): Vd[E, B] = this match { case Ok(a) => Ok(f(a)); case No(e) => No(e) }
  def zip[E2 >: E, B](o: Vd[E2, B]): Vd[E2, (A, B)] = (this, o) match
    case (Ok(a), Ok(b))   => Ok((a, b))
    case (No(e1), No(e2)) => No(e1 ++ e2)
    case (No(e), _)       => No(e)
    case (_, No(e))       => No(e)

object Vd:
  def ok[A](a: A): Vd[Nothing, A] = Ok(a)
  def no[E](e: E): Vd[E, Nothing] = No(List(e))
  def seq[E, A](vs: List[Vd[E, A]]): Vd[E, List[A]] = vs.foldRight(ok(List.empty[A]))((v, acc) => v.zip(acc).map(_ :: _))

// ═══════════════════════════════════════════════════════════════════════════
// ZIPPER + ATTRIBUTED: Both are just Co[V, A] with different A
// ═══════════════════════════════════════════════════════════════════════════

type Loc = (Val, List[Int])  // (focus, path)
type Zip = Co[V, Loc]        // Zipper = Cofree with location

object Z:  // Zipper ops
  def from(v: Val): Zip = 
    def go(v: Val, p: List[Int]): Zip =
      val ch = V.out(v) match
        case V.C(n, a) => V.C(n, a.zipWithIndex.map((c, i) => go(c, i :: p)))
        case V.S(s)    => V.S(s)
        case V.I(i)    => V.I(i)
        case V.L(l)    => V.L(l.zipWithIndex.map((c, i) => go(c, i :: p)))
      Co((v, p.reverse), ch)
    go(v, Nil)
  
  def nav(z: Zip, p: List[Int]): Option[Zip] = p match
    case Nil     => Some(z)
    case i :: ps => z.t match
      case V.C(_, a) if a.length > i => nav(a(i), ps)
      case V.L(l) if l.length > i    => nav(l(i), ps)
      case _                         => None
  
  def mod(z: Zip)(f: Val => Val): Zip = Co((f(z.h._1), z.h._2), z.t)
  def toVal(z: Zip): Val = Co.forget(z)

type Attr = Map[String, Any]  // Simple attribute map
type AVal = Co[V, Attr]       // Attributed = Cofree with attrs

object A:  // Attribute ops
  def down(v: Val)(f: (Val, Attr) => Attr): AVal =
    def go(v: Val, p: Attr): AVal =
      val a = f(v, p)
      Co(a, V.out(v) match
        case V.C(n, c) => V.C(n, c.map(go(_, a)))
        case V.S(s)    => V.S(s)
        case V.I(i)    => V.I(i)
        case V.L(l)    => V.L(l.map(go(_, a))))
    go(v, Map.empty)
  
  def up(v: Val)(f: (Val, List[Attr]) => Attr): AVal =
    def go(v: Val): AVal =
      val cs = V.out(v) match { case V.C(_, c) => c.map(go); case V.L(l) => l.map(go); case _ => Nil }
      Co(f(v, cs.map(_.h)), V.out(v) match
        case V.C(n, _) => V.C(n, cs); case V.S(s) => V.S(s); case V.I(i) => V.I(i); case V.L(_) => V.L(cs))
    go(v)

// ═══════════════════════════════════════════════════════════════════════════
// HASH: Content-addressing
// ═══════════════════════════════════════════════════════════════════════════

opaque type H = String  // Hash
object H:
  def apply(s: String): H = s
  def of(v: Val): H = v.toString.hashCode.toHexString.take(8)
  extension (h: H) def str: String = h

class Sto[A](hash: A => H):  // Store
  private var m: Map[H, A] = Map.empty
  def put(a: A): H = { val h = hash(a); if !m.contains(h) then m = m + (h -> a); h }
  def get(h: H): Option[A] = m.get(h)
  def size: Int = m.size

// ═══════════════════════════════════════════════════════════════════════════
// YONEDA: Map fusion
// ═══════════════════════════════════════════════════════════════════════════

trait Yo[X[_], A]:  // Yoneda
  def run[B](f: A => B): X[B]
  def map[B](g: A => B): Yo[X, B] = new Yo[X, B] { def run[C](h: B => C) = Yo.this.run(g andThen h) }
  def lower(using F[X]): X[A] = run(identity)

object Yo:
  def lift[X[_]: F, A](xa: X[A]): Yo[X, A] = new Yo[X, A] { def run[B](f: A => B) = summon[F[X]].map(xa)(f) }

// ═══════════════════════════════════════════════════════════════════════════
// EXPORTS: Single import point
// ═══════════════════════════════════════════════════════════════════════════

object φ:  // phi exports
  // Types
  type Fix[X[_]] = μ[X]; val Fix = μ
  type Free[X[_], A] = Fr[X, A]; val Free = Fr
  type Cofree[X[_], A] = Co[X, A]; val Cofree = Co
  type ValF[A] = V[A]; val ValF = V
  type Lens[S, A] = Ln[S, A]; val Lens = Ln
  type Prism[S, A] = Pr[S, A]; val Prism = Pr
  type Traversal[S, A] = Tr[S, A]
  type Xform[A, B] = X[A, B]; val Xform = X
  type Edit[A] = Ed[A]; val Edit = Ed
  type Validated[E, A] = Vd[E, A]; val Validated = Vd
  type Hash = H; val Hash = H
  type Store[A] = Sto[A]
  type Zipper = Zip; val Zipper = Z
  type Attributed = AVal; val Attributed = A
  type Yoneda[X[_], A] = Yo[X, A]; val Yoneda = Yo
  
  // Typeclasses
  type Functor[X[_]] = F[X]
  type Monad[X[_]] = M[X]
  type Comonad[X[_]] = W[X]
  type ~>[X[_], Y[_]] = phi.core.~>[X, Y]
  
  // Recursion schemes
  def cata[A](f: V[A] => A)(v: Val): A = μ.cataV(f)(v)
  def ana[A](f: A => V[A])(a: A): Val = μ.anaV(f)(a)
  
  // Optics
  def arg(n: Int) = Op.arg(n)
  def con(n: String) = Op.con(n)
  def children = Op.ch
  def everywhere(f: Val => Val) = Op.everywhere(f)
