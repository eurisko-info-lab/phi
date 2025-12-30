package phi.core

import phi.meta.Val
import phi.meta.Val.*
import phi.meta.Term
import phi.meta.Term.*

/**
 * Unified Core: All core abstractions derived from algebraic foundations.
 * 
 * This file replaces the scattered implementations with a unified approach
 * where everything flows from a few fundamental structures:
 * 
 *   Fix[F]     → recursive types (Val)
 *   Free[F,A]  → effect sequencing (Change, TermOp)
 *   Cofree[F,A]→ annotated trees (Attributed, Zipper-as-context)
 *   ~>         → structure-preserving maps (Xform)
 *   Yoneda     → optimization (fused traversals)
 */

// =============================================================================
// Foundational Typeclasses (minimal set)
// =============================================================================

trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]
  extension [A](fa: F[A]) def fmap[B](f: A => B): F[B] = map(fa)(f)

trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  extension [A](ma: M[A]) def >>=[B](f: A => M[B]): M[B] = flatMap(ma)(f)

trait Comonad[W[_]] extends Functor[W]:
  def extract[A](wa: W[A]): A
  def extend[A, B](wa: W[A])(f: W[A] => B): W[B]
  def duplicate[A](wa: W[A]): W[W[A]] = extend(wa)(identity)
  extension [A](wa: W[A]) def coflatMap[B](f: W[A] => B): W[B] = extend(wa)(f)

trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

// =============================================================================
// Pattern Functor: The Shape of Terms
// =============================================================================

/** One layer of Val structure - the pattern functor */
enum ValF[+A]:
  case ConF(name: String, args: List[A])
  case StrF(s: String)
  case IntF(n: Int)
  case ListF(elems: List[A])

object ValF:
  given Functor[ValF] with
    def map[A, B](fa: ValF[A])(f: A => B): ValF[B] = fa match
      case ConF(n, args)  => ConF(n, args.map(f))
      case StrF(s)        => StrF(s)
      case IntF(n)        => IntF(n)
      case ListF(elems)   => ListF(elems.map(f))
  
  def project(v: Val): ValF[Val] = v match
    case VCon(n, args)  => ConF(n, args)
    case VStr(s)        => StrF(s)
    case VInt(n)        => IntF(n)
    case VList(elems)   => ListF(elems)
  
  def embed(vf: ValF[Val]): Val = vf match
    case ConF(n, args)  => VCon(n, args)
    case StrF(s)        => VStr(s)
    case IntF(n)        => VInt(n)
    case ListF(elems)   => VList(elems)

// =============================================================================
// Fix Point: Explicit Recursion
// =============================================================================

/** Fixed point of a functor - explicit recursion */
case class Fix[F[_]](unfix: F[Fix[F]])

object Fix:
  /** Catamorphism: fold */
  def cata[F[_]: Functor, A](alg: F[A] => A)(fix: Fix[F]): A =
    alg(summon[Functor[F]].map(fix.unfix)(cata[F, A](alg)))
  
  /** Anamorphism: unfold */
  def ana[F[_]: Functor, A](coalg: A => F[A])(seed: A): Fix[F] =
    Fix(summon[Functor[F]].map(coalg(seed))(ana[F, A](coalg)))
  
  /** Hylomorphism: refold (fused unfold+fold) */
  def hylo[F[_]: Functor, A, B](alg: F[B] => B)(coalg: A => F[A])(seed: A): B =
    alg(summon[Functor[F]].map(coalg(seed))(hylo[F, A, B](alg)(coalg)))
  
  /** Paramorphism: fold with access to original */
  def para[F[_]: Functor, A](alg: F[(Fix[F], A)] => A)(fix: Fix[F]): A =
    alg(summon[Functor[F]].map(fix.unfix)(f => (f, para[F, A](alg)(f))))

// =============================================================================
// Free Monad: Effect Sequencing
// =============================================================================

/** Free monad over F - inspectable effect sequences */
enum Free[F[_], A]:
  case Pure(a: A)
  case Suspend(fa: F[Free[F, A]])

object Free:
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  
  def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] = 
    Suspend(summon[Functor[F]].map(fa)(Pure(_)))
  
  given [F[_]: Functor]: Monad[[A] =>> Free[F, A]] with
    def pure[A](a: A): Free[F, A] = Pure(a)
    def flatMap[A, B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma match
      case Pure(a)     => f(a)
      case Suspend(fa) => Suspend(summon[Functor[F]].map(fa)(_.flatMap(f)))
  
  extension [F[_]: Functor, A](ma: Free[F, A])
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = ma match
      case Pure(a)     => f(a)
      case Suspend(fa) => Suspend(summon[Functor[F]].map(fa)(_.flatMap(f)))
  
  /** Interpret into any monad via natural transformation */
  def foldMap[F[_]: Functor, M[_]: Monad, A](prog: Free[F, A])(nat: F ~> M): M[A] =
    prog match
      case Pure(a)     => summon[Monad[M]].pure(a)
      case Suspend(fa) => summon[Monad[M]].flatMap(nat(fa))(foldMap(_)(nat))

// =============================================================================
// Cofree Comonad: Annotated Trees
// =============================================================================

/** Cofree comonad over F - trees with annotations at every node */
case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

object Cofree:
  given [F[_]: Functor]: Comonad[[A] =>> Cofree[F, A]] with
    def map[A, B](fa: Cofree[F, A])(f: A => B): Cofree[F, B] =
      Cofree(f(fa.head), summon[Functor[F]].map(fa.tail)(map(_)(f)))
    def extract[A](wa: Cofree[F, A]): A = wa.head
    def extend[A, B](wa: Cofree[F, A])(f: Cofree[F, A] => B): Cofree[F, B] =
      Cofree(f(wa), summon[Functor[F]].map(wa.tail)(extend(_)(f)))
  
  /** Build annotated tree from Val */
  def annotate[A](v: Val)(f: Val => A): Cofree[ValF, A] =
    given Functor[ValF] = ValF.given_Functor_ValF
    Cofree(f(v), ValF.project(v) match
      case ValF.ConF(n, args)  => ValF.ConF(n, args.map(c => annotate[A](c)(f)))
      case ValF.StrF(s)        => ValF.StrF(s)
      case ValF.IntF(n)        => ValF.IntF(n)
      case ValF.ListF(elems)   => ValF.ListF(elems.map(c => annotate[A](c)(f)))
    )
  
  /** Extract underlying Val, discarding annotations */
  def forget(c: Cofree[ValF, ?]): Val =
    given Functor[ValF] = ValF.given_Functor_ValF
    ValF.embed(summon[Functor[ValF]].map(c.tail)(x => forget(x)))

// =============================================================================
// Unified Change: Simple edit operations
// =============================================================================

/** Edit operations - simplified without GADTs */
enum Edit[+A]:
  case Insert[V](value: V) extends Edit[V]
  case Delete(label: Option[String] = None) extends Edit[Nothing]
  case Replace[V](newVal: V) extends Edit[V]
  case AndThen[A, B](first: Edit[A], second: Edit[B]) extends Edit[B]
  case Pure[A](a: A) extends Edit[A]

object Edit:
  def insert[V](v: V): Edit[V] = Insert(v)
  def delete: Edit[Nothing] = Delete(None)
  def replace[V](v: V): Edit[V] = Replace(v)
  def pure[A](a: A): Edit[A] = Pure(a)
  def sequence[A](edits: Edit[A]*): Edit[A] = edits.reduceLeft((a, b) => AndThen(a, b))
  
  /** Apply edit to a Term */
  def apply[A](edit: Edit[A], term: Term[A]): Term[A] = edit match
    case Insert(v)      => term match { case Term.Hole(_) => Term.Done(v.asInstanceOf[A]); case _ => term }
    case Delete(l)      => Term.Hole(l)
    case Replace(v)     => Term.Done(v.asInstanceOf[A])
    case AndThen(f, s)  => apply(s, apply(f.asInstanceOf[Edit[A]], term))
    case Pure(a)        => Term.Done(a.asInstanceOf[A])
  
  /** Invert for undo */
  def invert[A](edit: Edit[A], original: Term[A]): Edit[A] = edit match
    case Insert(_)     => Delete(None).asInstanceOf[Edit[A]]
    case Delete(_)     => original match { case Term.Done(v) => Insert(v); case _ => Delete(None).asInstanceOf[Edit[A]] }
    case Replace(_)    => original match { case Term.Done(v) => Replace(v); case _ => Delete(None).asInstanceOf[Edit[A]] }
    case AndThen(f, s) => AndThen(invert(s, apply(f.asInstanceOf[Edit[A]], original)), invert(f.asInstanceOf[Edit[A]], original)).asInstanceOf[Edit[A]]
    case Pure(a)       => Pure(a)

// =============================================================================
// Unified Zipper: Cofree-based navigation context
// =============================================================================

/** Zipper context as a list monad of paths */
case class Loc[A](focus: A, path: List[Int])

/** Zipper using Cofree for context-aware navigation */
type ZipVal = Cofree[ValF, Loc[Val]]

object Zipper:
  /** Create zipper from Val */
  def fromVal(v: Val): ZipVal = 
    given Functor[ValF] = ValF.given_Functor_ValF
    def go(v: Val, path: List[Int]): ZipVal =
      val children = ValF.project(v) match
        case ValF.ConF(n, args) => ValF.ConF(n, args.zipWithIndex.map((c, i) => go(c, i :: path)))
        case ValF.StrF(s)       => ValF.StrF(s)
        case ValF.IntF(n)       => ValF.IntF(n)
        case ValF.ListF(elems)  => ValF.ListF(elems.zipWithIndex.map((c, i) => go(c, i :: path)))
      Cofree(Loc(v, path.reverse), children)
    go(v, Nil)
  
  /** Navigate to path */
  def navigateTo(z: ZipVal, path: List[Int]): Option[ZipVal] = path match
    case Nil => Some(z)
    case i :: rest => z.tail match
      case ValF.ConF(_, args) if args.length > i => navigateTo(args(i), rest)
      case ValF.ListF(elems) if elems.length > i => navigateTo(elems(i), rest)
      case _ => None
  
  /** Modify at current location */
  def modify(z: ZipVal)(f: Val => Val): ZipVal =
    given Functor[ValF] = ValF.given_Functor_ValF
    Cofree(Loc(f(z.head.focus), z.head.path), z.tail)
  
  /** To Val, discarding location info */
  def toVal(z: ZipVal): Val = Cofree.forget(z)

// =============================================================================
// Unified Attributes: Cofree with attribute maps
// =============================================================================

/** Attributes at a node */
case class Attrs(inherited: Map[String, Any] = Map.empty, synthesized: Map[String, Any] = Map.empty):
  def get(name: String): Option[Any] = synthesized.get(name).orElse(inherited.get(name))
  def setI(name: String, v: Any): Attrs = copy(inherited = inherited + (name -> v))
  def setS(name: String, v: Any): Attrs = copy(synthesized = synthesized + (name -> v))

/** Attributed tree = Cofree with Attrs annotation */
type AttrVal = Cofree[ValF, Attrs]

object Attributed:
  /** Compute inherited attributes (top-down) */
  def inheritDown(v: Val)(inherit: (Val, Attrs) => Attrs): AttrVal =
    given Functor[ValF] = ValF.given_Functor_ValF
    def go(v: Val, parentAttrs: Attrs): AttrVal =
      val attrs = inherit(v, parentAttrs)
      val children = ValF.project(v) match
        case ValF.ConF(n, args) => ValF.ConF(n, args.map(go(_, attrs)))
        case ValF.StrF(s)       => ValF.StrF(s)
        case ValF.IntF(n)       => ValF.IntF(n)
        case ValF.ListF(elems)  => ValF.ListF(elems.map(go(_, attrs)))
      Cofree(attrs, children)
    go(v, Attrs())
  
  /** Compute synthesized attributes (bottom-up) */
  def synthesizeUp(v: Val)(synth: (Val, List[Attrs]) => Attrs): AttrVal =
    given Functor[ValF] = ValF.given_Functor_ValF
    def go(v: Val): AttrVal =
      val childTrees = ValF.project(v) match
        case ValF.ConF(n, args) => args.map(go)
        case ValF.ListF(elems)  => elems.map(go)
        case _                  => Nil
      val childAttrs = childTrees.map(_.head)
      val children = ValF.project(v) match
        case ValF.ConF(n, _)   => ValF.ConF(n, childTrees)
        case ValF.StrF(s)      => ValF.StrF(s)
        case ValF.IntF(n)      => ValF.IntF(n)
        case ValF.ListF(_)     => ValF.ListF(childTrees)
      Cofree(synth(v, childAttrs), children)
    go(v)
  
  /** Full attribute grammar: both passes */
  def attribute(v: Val)(
    inherit: (Val, Attrs) => Attrs,
    synth: (Val, Attrs, List[Attrs]) => Attrs
  ): AttrVal =
    given Functor[ValF] = ValF.given_Functor_ValF
    def go(v: Val, parentAttrs: Attrs): AttrVal =
      val inh = inherit(v, parentAttrs)
      val childTrees = ValF.project(v) match
        case ValF.ConF(n, args) => args.map(go(_, inh))
        case ValF.ListF(elems)  => elems.map(go(_, inh))
        case _                  => Nil
      val childAttrs = childTrees.map(_.head)
      val attrs = synth(v, inh, childAttrs)
      val children = ValF.project(v) match
        case ValF.ConF(n, _)   => ValF.ConF(n, childTrees)
        case ValF.StrF(s)      => ValF.StrF(s)
        case ValF.IntF(n)      => ValF.IntF(n)
        case ValF.ListF(_)     => ValF.ListF(childTrees)
      Cofree(attrs, children)
    go(v, Attrs())

// =============================================================================
// Unified Xform: Natural transformation + Profunctor
// =============================================================================

/** Bidirectional transform as pair of natural transformations */
trait Xform[A, B]:
  def forward(a: A): Option[B]
  def backward(b: B): Option[A]
  
  def andThen[C](other: Xform[B, C]): Xform[A, C] = Xform.compose(this, other)
  def inverse: Xform[B, A] = Xform.invert(this)
  def lmap[A0](f: A0 => A): Xform[A0, B] = Xform.lmap(this)(f)
  def rmap[B0](g: B => B0): Xform[A, B0] = Xform.rmap(this)(g)

object Xform:
  def apply[A, B](fwd: A => Option[B], bwd: B => Option[A]): Xform[A, B] = new Xform[A, B]:
    def forward(a: A) = fwd(a)
    def backward(b: B) = bwd(b)
  
  def id[A]: Xform[A, A] = Xform(Some(_), Some(_))
  def compose[A, B, C](f: Xform[A, B], g: Xform[B, C]): Xform[A, C] =
    Xform(a => f.forward(a).flatMap(g.forward), c => g.backward(c).flatMap(f.backward))
  def invert[A, B](f: Xform[A, B]): Xform[B, A] = Xform(f.backward, f.forward)
  def lmap[A0, A, B](f: Xform[A, B])(g: A0 => A): Xform[A0, B] = Xform(a0 => f.forward(g(a0)), _ => None)
  def rmap[A, B, B0](f: Xform[A, B])(g: B => B0): Xform[A, B0] = Xform(a => f.forward(a).map(g), _ => None)

// =============================================================================
// Yoneda: Map Fusion
// =============================================================================

/** Yoneda[F, A] ≅ F[A] but fuses maps */
trait Yoneda[F[_], A]:
  def run[B](f: A => B): F[B]
  def map[B](g: A => B): Yoneda[F, B] = new Yoneda[F, B]:
    def run[C](h: B => C): F[C] = Yoneda.this.run(g andThen h)
  def lower(using F: Functor[F]): F[A] = run(identity)

object Yoneda:
  def lift[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] = new Yoneda[F, A]:
    def run[B](f: A => B): F[B] = summon[Functor[F]].map(fa)(f)

// =============================================================================
// Optics: Unified access patterns
// =============================================================================

/** Lens: exactly one focus */
case class Lens[S, A](get: S => A, set: A => S => S):
  def modify(f: A => A)(s: S): S = set(f(get(s)))(s)
  def andThen[B](other: Lens[A, B]): Lens[S, B] =
    Lens(s => other.get(get(s)), b => s => set(other.set(b)(get(s)))(s))

/** Prism: maybe one focus (pattern matching) */
case class Prism[S, A](getOption: S => Option[A], reverseGet: A => S):
  def modify(f: A => A)(s: S): S = getOption(s).map(a => reverseGet(f(a))).getOrElse(s)

/** Traversal: zero or more foci */
trait Traversal[S, A]:
  def getAll(s: S): List[A]
  def modify(f: A => A)(s: S): S

object Optics:
  /** Lens into VCon args */
  def arg(n: Int): Lens[Val, Val] = Lens(
    { case VCon(_, args) => args(n); case v => v },
    (v: Val) => { case VCon(name, args) => VCon(name, args.updated(n, v)); case s => s }
  )
  
  /** Prism for constructor matching */
  def con(name: String): Prism[Val, List[Val]] = Prism(
    (s: Val) => s match { case VCon(n, args) if n == name => Some(args); case _ => None },
    args => VCon(name, args)
  )
  
  /** Traversal over all children */
  val children: Traversal[Val, Val] = new Traversal[Val, Val]:
    def getAll(s: Val) = s match
      case VCon(_, args)  => args
      case VList(elems)   => elems
      case _              => Nil
    def modify(f: Val => Val)(s: Val) = s match
      case VCon(n, args)  => VCon(n, args.map(f))
      case VList(elems)   => VList(elems.map(f))
      case other          => other
  
  /** Deep traversal */
  def everywhere(f: Val => Val): Val => Val = v => f(children.modify(Optics.everywhere(f))(v))

// =============================================================================
// Validated: Parallel error collection
// =============================================================================

enum Validated[+E, +A]:
  case Valid(a: A)
  case Invalid(errors: List[E])
  
  def map[B](f: A => B): Validated[E, B] = this match
    case Valid(a)      => Valid(f(a))
    case Invalid(errs) => Invalid(errs)
  
  def zip[E2 >: E, B](other: Validated[E2, B]): Validated[E2, (A, B)] = (this, other) match
    case (Valid(a), Valid(b))       => Valid((a, b))
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (Invalid(e), _)            => Invalid(e)
    case (_, Invalid(e))            => Invalid(e)

object Validated:
  def valid[A](a: A): Validated[Nothing, A] = Valid(a)
  def invalid[E](e: E): Validated[E, Nothing] = Invalid(List(e))
  def sequence[E, A](vs: List[Validated[E, A]]): Validated[E, List[A]] =
    vs.foldRight(valid(List.empty[A]))((v, acc) => v.zip(acc).map(_ :: _))
  def map2[E, A, B, C](va: Validated[E, A], vb: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    va.zip(vb).map(f.tupled)

// =============================================================================
// Hash Consing: Content-addressed storage (simplified)
// =============================================================================

opaque type Hash = String
object Hash:
  def apply(s: String): Hash = s
  def compute(v: Val): Hash = 
    val content = v.toString
    content.hashCode.toHexString.take(8)
  extension (h: Hash) def value: String = h

class Store[A](hasher: A => Hash):
  private var store: Map[Hash, A] = Map.empty
  def intern(a: A): (Hash, A) =
    val h = hasher(a)
    store.get(h) match
      case Some(existing) => (h, existing)
      case None => store = store + (h -> a); (h, a)
  def get(h: Hash): Option[A] = store.get(h)
  def size: Int = store.size

// =============================================================================
// All-in-one exports
// =============================================================================

object Core:
  // Re-export everything for easy imports
  export ValF.*
  export Fix.*
  export Free.{Pure, Suspend, pure as freePure, liftF}
  export Cofree.{annotate, forget}
  export Edit.{insert, delete, replace, pure as editPure, sequence as editSeq}
  export Zipper.{fromVal, navigateTo, modify as zmodify, toVal}
  export Attributed.{inheritDown, synthesizeUp, attribute}
  export Xform.{id as xformId, compose, invert}
  export Optics.{arg, con, children, everywhere}
  export Validated.{valid, invalid, sequence as vsequence, map2 as vmap2}
  
  // Recursion schemes on Val
  def cataVal[A](alg: ValF[A] => A)(v: Val): A =
    given Functor[ValF] = ValF.given_Functor_ValF
    alg(summon[Functor[ValF]].map(ValF.project(v))(cataVal(alg)))
  
  def anaVal[A](coalg: A => ValF[A])(seed: A): Val =
    given Functor[ValF] = ValF.given_Functor_ValF
    ValF.embed(summon[Functor[ValF]].map(coalg(seed))(anaVal(coalg)))
