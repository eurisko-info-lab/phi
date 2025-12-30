package phi.core

import phi.meta.{Val, Term}
import Val.*
import Term.*

/**
 * Φ-CORE: Algebraic Foundation for Metaprogramming
 * 
 * This module provides the complete algebraic infrastructure for building
 * language tools: parsers, analyzers, transformers, and editors.
 * 
 * ARCHITECTURE
 * ============
 * 
 * Everything derives from a small set of mathematical structures:
 * 
 *   TYPECLASSES (4)
 *   ├── F[_]    Functor     - structure-preserving maps
 *   ├── M[_]    Monad       - sequenced computations  
 *   ├── W[_]    Comonad     - context-dependent computations
 *   └── ~>      Natural     - polymorphic transformations
 * 
 *   RECURSIVE STRUCTURES (4)
 *   ├── V[A]    ValF        - one layer of syntax (pattern functor)
 *   ├── μ[F]    Fix         - recursive types (Val = μ[V])
 *   ├── Fr[F,A] Free        - effect sequences (editable programs)
 *   └── Co[F,A] Cofree      - annotated trees (zippers, attributes)
 * 
 *   ACCESSORS (3)
 *   ├── Ln[S,A] Lens        - focus on exactly one part
 *   ├── Pr[S,A] Prism       - focus on one of many cases
 *   └── Tr[S,A] Traversal   - focus on zero or more parts
 * 
 *   TRANSFORMS (2)
 *   ├── X[A,B]  Xform       - bidirectional mappings
 *   └── Ed[A]   Edit        - algebraic change operations
 * 
 *   UTILITIES (4)
 *   ├── Vd[E,A] Validated   - parallel error accumulation
 *   ├── H       Hash        - content addressing
 *   ├── Sto[A]  Store       - hash-consed storage
 *   └── Yo[F,A] Yoneda      - map fusion optimization
 * 
 * KEY INSIGHT
 * ===========
 * 
 * Everything is a tree with annotations:
 * 
 *   - Zipper     = Co[V, (Val, Path)]     -- tree + location
 *   - Attributed = Co[V, Map[String,Any]] -- tree + computed values
 *   - Versioned  = Co[V, Hash]            -- tree + content hashes
 * 
 * The universal `Co.annotate[I,S]` function implements all attribute grammars
 * in a single pass: inherited attributes flow down, synthesized flow up.
 */

// ═══════════════════════════════════════════════════════════════════════════
// TYPECLASSES: The Four Fundamental Abstractions
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Functor: Structure-preserving transformation.
 * 
 * Laws:
 *   - map(fa)(identity) == fa
 *   - map(fa)(f andThen g) == map(map(fa)(f))(g)
 */
trait F[X[_]]:
  def map[A, B](xa: X[A])(f: A => B): X[B]

/**
 * Monad: Sequenced computations with flatMap.
 * 
 * Laws:
 *   - pure(a).flatMap(f) == f(a)
 *   - m.flatMap(pure) == m
 *   - m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))
 */
trait M[X[_]] extends F[X]:
  def pure[A](a: A): X[A]
  def bind[A, B](xa: X[A])(f: A => X[B]): X[B]
  
  override def map[A, B](xa: X[A])(f: A => B): X[B] = 
    bind(xa)(a => pure(f(a)))

/**
 * Comonad: Context-dependent computations with extract/extend.
 * 
 * Laws:
 *   - extract(extend(wa)(f)) == f(wa)
 *   - extend(wa)(extract) == wa
 *   - extend(extend(wa)(f))(g) == extend(wa)(w => g(extend(w)(f)))
 */
trait W[X[_]] extends F[X]:
  def extract[A](xa: X[A]): A
  def extend[A, B](xa: X[A])(f: X[A] => B): X[B]

/**
 * Natural Transformation: Polymorphic function between functors.
 * 
 * For all types A: nat(F[A]) => G[A]
 */
trait ~>[X[_], Y[_]]:
  def apply[A](xa: X[A]): Y[A]

// ═══════════════════════════════════════════════════════════════════════════
// V: Pattern Functor (One Layer of Val)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * The pattern functor for Val - represents one layer of the AST.
 * 
 * Val is conceptually the fixed point: Val ≅ V[Val]
 * 
 * This factoring enables:
 *   - Generic recursion schemes (cata, ana, hylo)
 *   - Layer-by-layer processing
 *   - Separation of recursion from algebra
 */
enum V[+A]:
  case C(name: String, args: List[A])  // Constructor: Foo(a, b, c)
  case S(value: String)                 // String literal
  case I(value: Int)                    // Integer literal
  case L(elements: List[A])             // List: [a, b, c]

object V:
  /** V is a Functor - we can map over children */
  given F[V] with
    def map[A, B](v: V[A])(f: A => B): V[B] = v match
      case C(n, args) => C(n, args.map(f))
      case S(s)       => S(s)
      case I(i)       => I(i)
      case L(elems)   => L(elems.map(f))
  
  /** Project: unfold one layer of Val into V[Val] */
  def out(v: Val): V[Val] = v match
    case VCon(n, args) => C(n, args)
    case VStr(s)       => S(s)
    case VInt(i)       => I(i)
    case VList(elems)  => L(elems)
  
  /** Embed: fold one layer of V[Val] back into Val */
  def in(v: V[Val]): Val = v match
    case C(n, args) => VCon(n, args)
    case S(s)       => VStr(s)
    case I(i)       => VInt(i)
    case L(elems)   => VList(elems)
  
  /** Get children as a list */
  def children(v: V[?]): List[?] = v match
    case C(_, args) => args
    case L(elems)   => elems
    case _          => Nil
  
  /** Map over children using the Functor instance */
  def mapChildren[A, B](v: V[A])(f: A => B): V[B] = 
    summon[F[V]].map(v)(f)

// ═══════════════════════════════════════════════════════════════════════════
// μ: Fix Point with Recursion Schemes
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Fixed point of a functor: μF = F[μF]
 * 
 * This represents recursive types explicitly, enabling generic
 * recursion schemes that work for any functor.
 */
case class μ[X[_]](unfix: X[μ[X]])

object μ:
  /**
   * Catamorphism: Generic fold.
   * 
   * Collapses a recursive structure bottom-up using an algebra.
   * Example: evaluate an expression tree to a value.
   */
  def cata[X[_]: F, A](algebra: X[A] => A)(term: μ[X]): A =
    algebra(summon[F[X]].map(term.unfix)(cata(algebra)))
  
  /**
   * Anamorphism: Generic unfold.
   * 
   * Builds a recursive structure top-down from a seed using a coalgebra.
   * Example: generate a tree from a grammar.
   */
  def ana[X[_]: F, A](coalgebra: A => X[A])(seed: A): μ[X] =
    μ(summon[F[X]].map(coalgebra(seed))(ana(coalgebra)))
  
  /**
   * Hylomorphism: Fused unfold-then-fold.
   * 
   * Builds and immediately consumes a structure without
   * materializing the intermediate result.
   */
  def hylo[X[_]: F, A, B](
    algebra: X[B] => B,
    coalgebra: A => X[A]
  )(seed: A): B =
    algebra(summon[F[X]].map(coalgebra(seed))(hylo(algebra, coalgebra)))
  
  // --- Val-specific schemes (since Val isn't defined as μ[V]) ---
  
  /** Catamorphism specialized for Val */
  def cataV[A](algebra: V[A] => A)(v: Val): A =
    algebra(V.mapChildren(V.out(v))(cataV(algebra)))
  
  /** Anamorphism specialized for Val */
  def anaV[A](coalgebra: A => V[A])(seed: A): Val =
    V.in(V.mapChildren(coalgebra(seed))(anaV(coalgebra)))

// ═══════════════════════════════════════════════════════════════════════════
// Fr: Free Monad
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Free monad over functor F.
 * 
 * Represents a computation as an inspectable data structure:
 *   - Pure(a): computation finished with value a
 *   - Suspend(fa): computation needs effect fa to continue
 * 
 * This enables:
 *   - Building DSLs as data
 *   - Multiple interpreters for the same program
 *   - Inspection and optimization before execution
 */
enum Fr[X[_], A]:
  case Pu(value: A)                    // Pure value
  case Su(suspended: X[Fr[X, A]])      // Suspended computation

object Fr:
  /** Lift a pure value into Free */
  def pure[X[_], A](a: A): Fr[X, A] = Pu(a)
  
  /** Lift an effect into Free */
  def lift[X[_]: F, A](xa: X[A]): Fr[X, A] = 
    Su(summon[F[X]].map(xa)(Pu(_)))
  
  /** Free[F, _] is a Monad when F is a Functor */
  given [X[_]: F]: M[[A] =>> Fr[X, A]] with
    def pure[A](a: A): Fr[X, A] = Pu(a)
    
    def bind[A, B](m: Fr[X, A])(f: A => Fr[X, B]): Fr[X, B] = m match
      case Pu(a)  => f(a)
      case Su(xa) => Su(summon[F[X]].map(xa)(bind(_)(f)))
  
  /**
   * Interpret Free into any monad via natural transformation.
   * 
   * This is the key to Free's power: write your program once,
   * run it with different interpreters.
   */
  def run[X[_]: F, Y[_]: M, A](program: Fr[X, A])(interpret: X ~> Y): Y[A] = 
    program match
      case Pu(a)  => summon[M[Y]].pure(a)
      case Su(xa) => summon[M[Y]].bind(interpret(xa))(run(_)(interpret))

// ═══════════════════════════════════════════════════════════════════════════
// Co: Cofree Comonad
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Cofree comonad over functor F.
 * 
 * A tree where every node carries an annotation:
 *   - h: the annotation at this node
 *   - t: the children (wrapped in F)
 * 
 * This is the dual of Free: where Free sequences effects,
 * Cofree decorates structure.
 * 
 * Applications:
 *   - Zippers: annotate with (value, path)
 *   - Attribute grammars: annotate with computed attributes
 *   - Versioning: annotate with content hashes
 */
case class Co[X[_], A](h: A, t: X[Co[X, A]])

object Co:
  /** Cofree[F, _] is a Comonad when F is a Functor */
  given [X[_]: F]: W[[A] =>> Co[X, A]] with
    def map[A, B](c: Co[X, A])(f: A => B): Co[X, B] =
      Co(f(c.h), summon[F[X]].map(c.t)(map(_)(f)))
    
    def extract[A](c: Co[X, A]): A = c.h
    
    def extend[A, B](c: Co[X, A])(f: Co[X, A] => B): Co[X, B] =
      Co(f(c), summon[F[X]].map(c.t)(extend(_)(f)))
  
  /**
   * Universal attribute grammar: computes annotations in one pass.
   * 
   * This single function subsumes all attribute grammars:
   *   - inh: compute inherited attributes (flowing down from parent)
   *   - syn: compute synthesized attributes (flowing up from children)
   *   - i0: initial inherited attribute at root
   * 
   * The magic: both directions happen in ONE traversal.
   */
  def annotate[I, S](v: Val)(
    inherit: (Val, I) => I,
    synthesize: (Val, I, List[S]) => S,
    initial: I
  ): Co[V, S] =
    def go(v: Val, inherited: I): Co[V, S] =
      // Compute inherited attribute for children
      val childInherited = inherit(v, inherited)
      
      // Get the structure
      val structure = V.out(v)
      
      // Recursively annotate children
      val annotatedChildren: List[Co[V, S]] = structure match
        case V.C(_, args) => args.map(go(_, childInherited))
        case V.L(elems)   => elems.map(go(_, childInherited))
        case _            => Nil
      
      // Compute synthesized attribute from children's attributes
      val synthesized = synthesize(v, childInherited, annotatedChildren.map(_.h))
      
      // Reconstruct structure with annotated children
      val newStructure = structure match
        case V.C(n, _) => V.C(n, annotatedChildren)
        case V.L(_)    => V.L(annotatedChildren)
        case V.S(s)    => V.S(s)
        case V.I(i)    => V.I(i)
      
      Co(synthesized, newStructure)
    
    go(v, initial)
  
  /** Strip annotations to recover the original Val */
  def forget(c: Co[V, ?]): Val = 
    V.in(V.mapChildren(c.t)(forget))

// ═══════════════════════════════════════════════════════════════════════════
// OPTICS: Lens, Prism, Traversal
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Lens: Focus on exactly one part of a structure.
 * 
 * A lens from S to A provides:
 *   - get: extract A from S
 *   - set: replace A in S
 * 
 * Laws:
 *   - get(set(a)(s)) == a
 *   - set(get(s))(s) == s
 *   - set(a)(set(b)(s)) == set(a)(s)
 */
case class Ln[S, A](get: S => A, set: A => S => S):
  /** Modify the focus using a function */
  def modify(f: A => A)(s: S): S = set(f(get(s)))(s)
  
  /** Compose with another lens */
  def >>[B](other: Ln[A, B]): Ln[S, B] = Ln(
    s => other.get(get(s)),
    b => s => set(other.set(b)(get(s)))(s)
  )

/**
 * Prism: Focus on one case of a sum type.
 * 
 * A prism from S to A provides:
 *   - getOption: try to extract A from S
 *   - reverseGet: construct S from A
 * 
 * Example: Prism[Val, String] focusing on VStr case
 */
case class Pr[S, A](getOption: S => Option[A], reverseGet: A => S):
  /** Modify if the prism matches */
  def modify(f: A => A)(s: S): S = 
    getOption(s).map(a => reverseGet(f(a))).getOrElse(s)

/**
 * Traversal: Focus on zero or more parts.
 * 
 * A traversal provides:
 *   - getAll: extract all focused values
 *   - modify: transform all focused values
 */
trait Tr[S, A]:
  def getAll(s: S): List[A]
  def modify(f: A => A)(s: S): S

/** Optics for Val */
object Op:
  /** Lens into the nth argument of a constructor */
  def arg(n: Int): Ln[Val, Val] = Ln(
    get = {
      case VCon(_, args) => args(n)
      case v => v
    },
    set = (newArg: Val) => {
      case VCon(name, args) => VCon(name, args.updated(n, newArg))
      case s => s
    }
  )
  
  /** Prism matching a specific constructor */
  def con(name: String): Pr[Val, List[Val]] = Pr(
    getOption = {
      case VCon(n, args) if n == name => Some(args)
      case _ => None
    },
    reverseGet = args => VCon(name, args)
  )
  
  /** Traversal over all immediate children */
  val children: Tr[Val, Val] = new Tr[Val, Val]:
    def getAll(s: Val): List[Val] = V.children(V.out(s)).asInstanceOf[List[Val]]
    
    def modify(f: Val => Val)(s: Val): Val = 
      V.in(V.mapChildren(V.out(s))(f))
  
  /** Transform everywhere in the tree (bottom-up) */
  def everywhere(f: Val => Val): Val => Val = 
    v => f(children.modify(everywhere(f))(v))

// ═══════════════════════════════════════════════════════════════════════════
// X: Bidirectional Transform (Xform)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Bidirectional transformation between A and B.
 * 
 * Unlike a simple function, Xform captures both directions,
 * enabling round-tripping and synchronization.
 */
trait X[A, B]:
  def forward(a: A): Option[B]
  def backward(b: B): Option[A]
  
  /** Compose with another transform */
  def >>>[C](other: X[B, C]): X[A, C] = X.compose(this, other)
  
  /** Reverse the transformation */
  def inverse: X[B, A] = X(backward, forward)

object X:
  def apply[A, B](fwd: A => Option[B], bwd: B => Option[A]): X[A, B] = 
    new X[A, B]:
      def forward(a: A) = fwd(a)
      def backward(b: B) = bwd(b)
  
  /** Identity transform */
  def id[A]: X[A, A] = X(Some(_), Some(_))
  
  /** Compose two transforms */
  def compose[A, B, C](f: X[A, B], g: X[B, C]): X[A, C] = X(
    a => f.forward(a).flatMap(g.forward),
    c => g.backward(c).flatMap(f.backward)
  )

// ═══════════════════════════════════════════════════════════════════════════
// Ed: Edit Algebra
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Algebraic edit operations.
 * 
 * Edits form a monoid under sequencing, enabling:
 *   - Composition of changes
 *   - Undo via inversion
 *   - Collaborative editing via operational transform
 */
enum Ed[+A]:
  case Ins[V](value: V) extends Ed[V]   // Insert value
  case Del extends Ed[Nothing]           // Delete
  case Rep[V](value: V) extends Ed[V]   // Replace with value
  case Seq[A, B](first: Ed[A], second: Ed[B]) extends Ed[B]  // Sequence

object Ed:
  def insert[A](a: A): Ed[A] = Ins(a)
  def delete: Ed[Nothing] = Del
  def replace[A](a: A): Ed[A] = Rep(a)
  
  /** Apply an edit to a term */
  def apply[A](edit: Ed[A], term: Term[A]): Term[A] = edit match
    case Ins(v) => term match
      case Term.Hole(_) => Term.Done(v.asInstanceOf[A])
      case _ => term
    case Del => Term.Hole(None)
    case Rep(v) => Term.Done(v.asInstanceOf[A])
    case Seq(a, b) => apply(b, apply(a.asInstanceOf[Ed[A]], term))

// ═══════════════════════════════════════════════════════════════════════════
// Vd: Validated (Parallel Error Accumulation)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Validated: Either-like but accumulates errors.
 * 
 * Unlike Either which short-circuits on first error,
 * Validated collects ALL errors via the zip operation.
 * 
 * This enables parallel validation and comprehensive error reporting.
 */
enum Vd[+E, +A]:
  case Ok(value: A)
  case No(errors: List[E])
  
  def map[B](f: A => B): Vd[E, B] = this match
    case Ok(a)    => Ok(f(a))
    case No(errs) => No(errs)
  
  /** Combine two validations, accumulating errors */
  def zip[E2 >: E, B](other: Vd[E2, B]): Vd[E2, (A, B)] = (this, other) match
    case (Ok(a), Ok(b))     => Ok((a, b))
    case (No(e1), No(e2))   => No(e1 ++ e2)  // Accumulate!
    case (No(e), _)         => No(e)
    case (_, No(e))         => No(e)

object Vd:
  def ok[A](a: A): Vd[Nothing, A] = Ok(a)
  def error[E](e: E): Vd[E, Nothing] = No(List(e))
  
  /** Sequence a list of validations */
  def sequence[E, A](validations: List[Vd[E, A]]): Vd[E, List[A]] =
    validations.foldRight(ok(List.empty[A])) { (v, acc) =>
      v.zip(acc).map(_ :: _)
    }

// ═══════════════════════════════════════════════════════════════════════════
// H + Sto: Content Addressing
// ═══════════════════════════════════════════════════════════════════════════

/** Content hash - opaque wrapper around String */
opaque type H = String

object H:
  def apply(s: String): H = s
  
  /** Compute hash of a Val */
  def of(v: Val): H = v.toString.hashCode.toHexString.take(8)
  
  extension (h: H)
    def value: String = h

/**
 * Hash-consed store: deduplicates identical values.
 * 
 * Storing the same value twice returns the same hash,
 * enabling structural sharing and efficient comparison.
 */
class Sto[A](hash: A => H):
  private var storage: Map[H, A] = Map.empty
  
  /** Store a value, returning its hash */
  def put(a: A): H =
    val h = hash(a)
    if !storage.contains(h) then
      storage = storage + (h -> a)
    h
  
  /** Retrieve a value by hash */
  def get(h: H): Option[A] = storage.get(h)
  
  /** Number of unique values stored */
  def size: Int = storage.size

// ═══════════════════════════════════════════════════════════════════════════
// Yo: Yoneda (Map Fusion)
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Yoneda lemma: Yo[F, A] ≅ F[A]
 * 
 * But with a twist: multiple maps fuse into a single traversal.
 * 
 * Instead of: fa.map(f).map(g).map(h)  -- 3 traversals
 * Use:        Yo.lift(fa).map(f).map(g).map(h).lower -- 1 traversal
 */
trait Yo[X[_], A]:
  def run[B](f: A => B): X[B]
  
  def map[B](g: A => B): Yo[X, B] = new Yo[X, B]:
    def run[C](h: B => C): X[C] = Yo.this.run(g andThen h)
  
  def lower(using F[X]): X[A] = run(identity)

object Yo:
  def lift[X[_]: F, A](xa: X[A]): Yo[X, A] = new Yo[X, A]:
    def run[B](f: A => B): X[B] = summon[F[X]].map(xa)(f)

// ═══════════════════════════════════════════════════════════════════════════
// Z: Zipper (Navigation with Context)
// ═══════════════════════════════════════════════════════════════════════════

/** Location: current value plus path from root */
type Loc = (Val, List[Int])

/** Zipper: Cofree with location annotations */
type Zip = Co[V, Loc]

/** Zipper operations */
object Z:
  /** Create a zipper from a Val */
  def from(v: Val): Zip =
    def go(v: Val, path: List[Int]): Zip =
      val children = V.out(v) match
        case V.C(n, args) => V.C(n, args.zipWithIndex.map((c, i) => go(c, path :+ i)))
        case V.L(elems)   => V.L(elems.zipWithIndex.map((c, i) => go(c, path :+ i)))
        case V.S(s)       => V.S(s)
        case V.I(i)       => V.I(i)
      Co((v, path), children)
    go(v, Nil)
  
  /** Navigate to a path */
  def navigate(z: Zip, path: List[Int]): Option[Zip] = path match
    case Nil => Some(z)
    case i :: rest => z.t match
      case V.C(_, args) if args.length > i => navigate(args(i), rest)
      case V.L(elems) if elems.length > i  => navigate(elems(i), rest)
      case _ => None
  
  /** Modify the value at current location */
  def modify(z: Zip)(f: Val => Val): Zip = 
    Co((f(z.h._1), z.h._2), z.t)
  
  /** Convert back to Val */
  def toVal(z: Zip): Val = Co.forget(z)

// ═══════════════════════════════════════════════════════════════════════════
// A: Attributed Trees
// ═══════════════════════════════════════════════════════════════════════════

/** Attribute map */
type Attr = Map[String, Any]

/** Attributed tree: Cofree with attribute annotations */
type AVal = Co[V, Attr]

/** Attribute operations */
object A:
  /** Compute attributes top-down (inherited) */
  def inheritDown(v: Val)(compute: (Val, Attr) => Attr): AVal =
    Co.annotate[Attr, Attr](v)(
      inherit = (v, parent) => compute(v, parent),
      synthesize = (_, attrs, _) => attrs,
      initial = Map.empty
    )
  
  /** Compute attributes bottom-up (synthesized) */
  def synthesizeUp(v: Val)(compute: (Val, List[Attr]) => Attr): AVal =
    Co.annotate[Unit, Attr](v)(
      inherit = (_, _) => (),
      synthesize = (v, _, childAttrs) => compute(v, childAttrs),
      initial = ()
    )

// ═══════════════════════════════════════════════════════════════════════════
// Φ: Unified Exports
// ═══════════════════════════════════════════════════════════════════════════

/**
 * Single import point for all Φ-core functionality.
 * 
 * Usage:
 *   import phi.core.Φ.*
 *   
 *   val result = cata { case C("Add", List(a, b)) => a + b; ... }(expr)
 */
object Φ:
  // Type aliases for readability
  type Fix[X[_]] = μ[X]
  type Free[X[_], A] = Fr[X, A]
  type Cofree[X[_], A] = Co[X, A]
  type ValF[A] = V[A]
  type Lens[S, A] = Ln[S, A]
  type Prism[S, A] = Pr[S, A]
  type Traversal[S, A] = Tr[S, A]
  type Xform[A, B] = X[A, B]
  type Edit[A] = Ed[A]
  type Validated[E, A] = Vd[E, A]
  type Hash = H
  type Store[A] = Sto[A]
  type Zipper = Zip
  type Attributed = AVal
  type Yoneda[X[_], A] = Yo[X, A]
  type Functor[X[_]] = F[X]
  type Monad[X[_]] = M[X]
  type Comonad[X[_]] = W[X]
  type ~>[X[_], Y[_]] = phi.core.~>[X, Y]
  
  // Companion objects
  val Fix = μ
  val Free = Fr
  val Cofree = Co
  val ValF = V
  val Lens = Ln
  val Prism = Pr
  val Xform = X
  val Edit = Ed
  val Validated = Vd
  val Hash = H
  val Zipper = Z
  val Attributed = A
  val Yoneda = Yo
  
  // Recursion schemes
  def cata[A](algebra: V[A] => A)(v: Val): A = μ.cataV(algebra)(v)
  def ana[A](coalgebra: A => V[A])(seed: A): Val = μ.anaV(coalgebra)(seed)
  
  // Optics
  def arg(n: Int): Lens[Val, Val] = Op.arg(n)
  def con(name: String): Prism[Val, List[Val]] = Op.con(name)
  def children: Traversal[Val, Val] = Op.children
  def everywhere(f: Val => Val): Val => Val = Op.everywhere(f)
