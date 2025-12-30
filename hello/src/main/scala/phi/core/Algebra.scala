package phi.core

import phi.meta.Term
import phi.meta.Term.*

/**
 * Additional algebraic structure on Term[A] and related types.
 * These come "for free" from the mathematical foundations.
 */

// =============================================================================
// 0. Base extension: map for Term (Functor)
// =============================================================================

/**
 * Term forms a Functor - this is the foundation for all other structures.
 */
extension [A](term: Term[A])
  def map[B](f: A => B): Term[B] = term match
    case Done(a) => Done(f(a))
    case Hole(l) => Hole(l)

// =============================================================================
// 1. Monad instance for Term[A]
// =============================================================================

/**
 * Term forms a Monad, giving us flatMap, sequence, traverse for free.
 */
object TermMonad:
  extension [A](term: Term[A])
    /** Monadic bind - chain computations that may produce holes */
    def flatMap[B](f: A => Term[B]): Term[B] = term match
      case Done(a)   => f(a)
      case Hole(l)   => Hole(l)
    
    /** Flatten nested Terms */
    def flatten[B](using ev: A <:< Term[B]): Term[B] = 
      term.flatMap(a => ev(a))
    
    /** Apply a Term[A => B] to a Term[A] */
    def ap[B](tf: Term[A => B]): Term[B] = (tf, term) match
      case (Done(f), Done(a)) => Done(f(a))
      case (Hole(l), _)       => Hole(l)
      case (_, Hole(l))       => Hole(l)
    
    /** Product of two Terms - both must succeed */
    def zip[B](other: Term[B]): Term[(A, B)] = (term, other) match
      case (Done(a), Done(b)) => Done((a, b))
      case (Hole(l), _)       => Hole(l)
      case (_, Hole(l))       => Hole(l)
  
  /** Lift a pure value into Term */
  def pure[A](a: A): Term[A] = Done(a)
  
  /** Sequence a list of Terms into a Term of list */
  def sequence[A](terms: List[Term[A]]): Term[List[A]] =
    terms.foldRight(pure(List.empty[A])) { (ta, acc) =>
      ta.zip(acc).map { case (a, as) => a :: as }
    }
  
  /** Traverse a list with a Term-producing function */
  def traverse[A, B](list: List[A])(f: A => Term[B]): Term[List[B]] =
    sequence(list.map(f))
  
  /** mapN for combining multiple Terms */
  def map2[A, B, C](ta: Term[A], tb: Term[B])(f: (A, B) => C): Term[C] =
    ta.zip(tb).map(f.tupled)
  
  def map3[A, B, C, D](ta: Term[A], tb: Term[B], tc: Term[C])(f: (A, B, C) => D): Term[D] =
    ta.zip(tb).zip(tc).map { case ((a, b), c) => f(a, b, c) }

// =============================================================================
// 2. Comonad instance for TermZipper
// =============================================================================

/**
 * TermZipper forms a Comonad, giving us context-aware transformations.
 */
object ZipperComonad:
  extension [A](zipper: TermZipper[A])
    /** Extract the focused value (counit) */
    def extract: Term[A] = zipper.focus
    
    /** Duplicate - create a zipper of zippers (comultiplication) */
    def duplicate: TermZipper[TermZipper[A]] =
      TermZipper(Done(zipper), zipper.context.asInstanceOf[List[ZipperContext[TermZipper[A]]]])
    
    /** Extend - apply a context-aware function to every position */
    def extend[B](f: TermZipper[A] => B): TermZipper[B] =
      TermZipper(Done(f(zipper)), zipper.context.asInstanceOf[List[ZipperContext[B]]])
    
    /** Map with access to context (like paramorphism) */
    def coflatMap[B](f: TermZipper[A] => B): TermZipper[B] = extend(f)
    
    /** Get depth from root */
    def depth: Int = zipper.context.length
    
    /** Get path from root as list of indices */
    def path: List[Int] = zipper.context.length :: Nil // simplified

// =============================================================================
// 3. Recursion Schemes
// =============================================================================

import phi.meta.Val
import phi.meta.Val.*

/**
 * Recursion schemes for Val (the term algebra).
 * These generalize folds and unfolds over algebraic structures.
 */
object RecursionSchemes:
  /** Pattern functor for Val - one layer of structure */
  enum ValF[+A]:
    case ConF(name: String, args: List[A])
    case StrF(s: String)
    case IntF(n: Int)
    case ListF(elems: List[A])
  
  import ValF.*
  
  /** Project: Val → ValF[Val] (one step of unfolding) */
  def project(v: Val): ValF[Val] = v match
    case VCon(name, args) => ConF(name, args)
    case VStr(s)          => StrF(s)
    case VInt(n)          => IntF(n)
    case VList(elems)     => ListF(elems)
  
  /** Embed: ValF[Val] → Val (one step of folding) */
  def embed(vf: ValF[Val]): Val = vf match
    case ConF(name, args) => VCon(name, args)
    case StrF(s)          => VStr(s)
    case IntF(n)          => VInt(n)
    case ListF(elems)     => VList(elems)
  
  /** Functor map for ValF */
  def mapF[A, B](vf: ValF[A])(f: A => B): ValF[B] = vf match
    case ConF(name, args) => ConF(name, args.map(f))
    case StrF(s)          => StrF(s)
    case IntF(n)          => IntF(n)
    case ListF(elems)     => ListF(elems.map(f))
  
  // -------------------------------------------------------------------------
  // Catamorphism (fold) - consume structure
  // -------------------------------------------------------------------------
  
  /** Catamorphism: fold a Val using an algebra */
  def cata[A](algebra: ValF[A] => A)(v: Val): A =
    algebra(mapF(project(v))(cata(algebra)))
  
  /** Example: count all nodes */
  val countNodes: Val => Int = cata {
    case ConF(_, args)  => 1 + args.sum
    case StrF(_)        => 1
    case IntF(_)        => 1
    case ListF(elems)   => 1 + elems.sum
  }
  
  /** Example: collect all strings */
  val collectStrings: Val => List[String] = cata {
    case ConF(name, args) => name :: args.flatten
    case StrF(s)          => List(s)
    case IntF(_)          => Nil
    case ListF(elems)     => elems.flatten
  }
  
  // -------------------------------------------------------------------------
  // Anamorphism (unfold) - produce structure
  // -------------------------------------------------------------------------
  
  /** Anamorphism: unfold a seed into a Val using a coalgebra */
  def ana[S](coalgebra: S => ValF[S])(seed: S): Val =
    embed(mapF(coalgebra(seed))(ana(coalgebra)))
  
  /** Example: generate a number as Church-style Val */
  def churchNumeral(n: Int): Val = ana[Int] { k =>
    if k <= 0 then ConF("Zero", Nil)
    else ConF("Succ", List(k - 1))
  }(n)
  
  // -------------------------------------------------------------------------
  // Hylomorphism (refold) - unfold then fold without intermediate
  // -------------------------------------------------------------------------
  
  /** Hylomorphism: unfold then fold, fused */
  def hylo[A, S](algebra: ValF[A] => A)(coalgebra: S => ValF[S])(seed: S): A =
    algebra(mapF(coalgebra(seed))(hylo(algebra)(coalgebra)))
  
  // -------------------------------------------------------------------------
  // Paramorphism - fold with access to original subterms
  // -------------------------------------------------------------------------
  
  /** Paramorphism: fold with access to original subterms */
  def para[A](algebra: ValF[(Val, A)] => A)(v: Val): A =
    algebra(mapF(project(v))(child => (child, para(algebra)(child))))
  
  /** Example: collect strings with their parent constructor */
  val stringsWithParent: Val => List[(String, String)] = para {
    case ConF(name, args) => args.flatMap(_._2)
    case StrF(s)          => Nil // can't see parent from here
    case IntF(_)          => Nil
    case ListF(elems)     => elems.flatMap(_._2)
  }

// =============================================================================
// 4. Optics (Lens/Prism/Traversal)
// =============================================================================

/**
 * Optics for accessing and modifying parts of terms.
 * Unifies Zipper, Change, and field access patterns.
 */
object Optics:
  /** A Lens focuses on exactly one A inside S */
  trait Lens[S, A]:
    def get(s: S): A
    def set(a: A)(s: S): S
    def modify(f: A => A)(s: S): S = set(f(get(s)))(s)
    
    def andThen[B](other: Lens[A, B]): Lens[S, B] = Lens.compose(this, other)
  
  object Lens:
    def apply[S, A](getter: S => A, setter: A => S => S): Lens[S, A] = new Lens[S, A]:
      def get(s: S): A = getter(s)
      def set(a: A)(s: S): S = setter(a)(s)
    
    def compose[S, A, B](outer: Lens[S, A], inner: Lens[A, B]): Lens[S, B] =
      Lens(
        s => inner.get(outer.get(s)),
        b => s => outer.modify(inner.set(b))(s)
      )
  
  /** A Prism focuses on an A that may or may not be in S (like pattern matching) */
  trait Prism[S, A]:
    def getOption(s: S): Option[A]
    def reverseGet(a: A): S
    def modify(f: A => A)(s: S): S = getOption(s).map(a => reverseGet(f(a))).getOrElse(s)
    
    def andThen[B](other: Prism[A, B]): Prism[S, B] = Prism.compose(this, other)
  
  object Prism:
    def apply[S, A](get: S => Option[A], reverse: A => S): Prism[S, A] = new Prism[S, A]:
      def getOption(s: S): Option[A] = get(s)
      def reverseGet(a: A): S = reverse(a)
    
    def compose[S, A, B](outer: Prism[S, A], inner: Prism[A, B]): Prism[S, B] =
      Prism(
        s => outer.getOption(s).flatMap(inner.getOption),
        b => outer.reverseGet(inner.reverseGet(b))
      )
  
  /** A Traversal focuses on zero or more A's inside S */
  trait Traversal[S, A]:
    def getAll(s: S): List[A]
    def modify(f: A => A)(s: S): S
    
    def andThen[B](other: Traversal[A, B]): Traversal[S, B] = Traversal.compose(this, other)
  
  object Traversal:
    def compose[S, A, B](outer: Traversal[S, A], inner: Traversal[A, B]): Traversal[S, B] =
      new Traversal[S, B]:
        def getAll(s: S): List[B] = outer.getAll(s).flatMap(inner.getAll)
        def modify(f: B => B)(s: S): S = outer.modify(inner.modify(f))(s)
  
  // Val-specific optics
  
  /** Prism for matching a specific constructor */
  def constructor(name: String): Prism[Val, List[Val]] = Prism(
    {
      case VCon(n, args) if n == name => Some(args)
      case _ => None
    },
    args => VCon(name, args)
  )
  
  /** Lens for the nth argument of a VCon */
  def arg(n: Int): Lens[List[Val], Val] = Lens(
    args => args(n),
    v => args => args.updated(n, v)
  )
  
  /** Traversal for all arguments */
  def allArgs: Traversal[Val, Val] = new Traversal[Val, Val]:
    def getAll(v: Val): List[Val] = v match
      case VCon(_, args) => args
      case VList(elems)  => elems
      case _             => Nil
    def modify(f: Val => Val)(v: Val): Val = v match
      case VCon(n, args) => VCon(n, args.map(f))
      case VList(elems)  => VList(elems.map(f))
      case other         => other
  
  /** Traversal for all descendants (deep) */
  def everywhere: Traversal[Val, Val] = new Traversal[Val, Val]:
    def getAll(v: Val): List[Val] = v :: (v match
      case VCon(_, args) => args.flatMap(getAll)
      case VList(elems)  => elems.flatMap(getAll)
      case _             => Nil
    )
    def modify(f: Val => Val)(v: Val): Val = f(v match
      case VCon(n, args) => VCon(n, args.map(modify(f)))
      case VList(elems)  => VList(elems.map(modify(f)))
      case other         => other
    )

// =============================================================================
// 5. Validated (Applicative for parallel error collection)
// =============================================================================

/**
 * Validated collects ALL errors rather than failing fast.
 * Forms an Applicative (but not a Monad).
 */
enum Validated[+E, +A]:
  case Valid(value: A)
  case Invalid(errors: List[E])
  
  def map[B](f: A => B): Validated[E, B] = this match
    case Valid(a)      => Valid(f(a))
    case Invalid(errs) => Invalid(errs)
  
  def zip[E2 >: E, B](other: Validated[E2, B]): Validated[E2, (A, B)] = (this, other) match
    case (Valid(a), Valid(b))           => Valid((a, b))
    case (Invalid(e1), Invalid(e2))     => Invalid(e1 ++ e2)  // collect both!
    case (Invalid(e), _)                => Invalid(e)
    case (_, Invalid(e))                => Invalid(e)
  
  def toEither: Either[List[E], A] = this match
    case Valid(a)      => Right(a)
    case Invalid(errs) => Left(errs)

object Validated:
  def valid[A](a: A): Validated[Nothing, A] = Valid(a)
  def invalid[E](e: E): Validated[E, Nothing] = Invalid(List(e))
  def invalidAll[E](es: List[E]): Validated[E, Nothing] = Invalid(es)
  
  def fromEither[E, A](either: Either[E, A]): Validated[E, A] = either match
    case Right(a) => Valid(a)
    case Left(e)  => Invalid(List(e))
  
  def fromOption[E, A](opt: Option[A], ifNone: => E): Validated[E, A] = opt match
    case Some(a) => Valid(a)
    case None    => Invalid(List(ifNone))
  
  /** Sequence a list of Validated, collecting all errors */
  def sequence[E, A](vs: List[Validated[E, A]]): Validated[E, List[A]] =
    vs.foldRight(valid(List.empty[A])) { (va, acc) =>
      va.zip(acc).map { case (a, as) => a :: as }
    }
  
  /** mapN combinators */
  def map2[E, A, B, C](va: Validated[E, A], vb: Validated[E, B])(f: (A, B) => C): Validated[E, C] =
    va.zip(vb).map(f.tupled)
  
  def map3[E, A, B, C, D](va: Validated[E, A], vb: Validated[E, B], vc: Validated[E, C])(f: (A, B, C) => D): Validated[E, D] =
    va.zip(vb).zip(vc).map { case ((a, b), c) => f(a, b, c) }

// =============================================================================
// Usage Examples
// =============================================================================

object AlgebraExamples:
  import TermMonad.*
  import RecursionSchemes.*
  import Optics.*
  
  // Example 1: Monadic parsing pipeline
  def pipeline(input: String): Term[String] =
    for
      parsed    <- parse(input)       // Term[Val]
      validated <- validate(parsed)   // Term[Val]  
      output    <- render(validated)  // Term[String]
    yield output
  
  def parse(s: String): Term[Val] = Done(VCon("Parsed", Nil))
  def validate(v: Val): Term[Val] = Done(v)
  def render(v: Val): Term[String] = Done("rendered")
  
  // Example 2: Recursion scheme - depth of term
  val depth: Val => Int = cata {
    case ValF.ConF(_, args)  => if args.isEmpty then 1 else 1 + args.max
    case ValF.StrF(_)        => 1
    case ValF.IntF(_)        => 1
    case ValF.ListF(elems)   => if elems.isEmpty then 1 else 1 + elems.max
  }
  
  // Example 3: Optics - modify all StringLits
  def doubleStrings(v: Val): Val =
    everywhere.modify {
      case VCon("StringLit", List(VStr(s))) => VCon("StringLit", List(VStr(s + s)))
      case other => other
    }(v)
  
  // Example 4: Parallel validation  
  def validateAll(checks: List[Val => Validated[String, Unit]])(v: Val): Validated[String, Val] =
    Validated.sequence(checks.map(_(v))).map(_ => v)
