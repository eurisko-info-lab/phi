package phi.core

import phi.meta.Val
import phi.meta.Val.*
import phi.meta.Term
import phi.meta.Term.*

/**
 * Advanced algebraic structures - the "next level".
 * These unlock even more powerful compositional patterns.
 */

// =============================================================================
// 1. Free Monad - Explicit effect sequencing
// =============================================================================

/**
 * Free[F, A] is the free monad over functor F.
 * It makes the sequencing of F-effects explicit and inspectable.
 * 
 * Key insight: Free[F, A] ≅ A + F[Free[F, A]]
 * - Pure(a): computation finished with result a
 * - Suspend(fa): one layer of F, then continue
 */
enum Free[F[_], A]:
  case Pure(a: A)
  case Suspend(fa: F[Free[F, A]])
  
  def map[B](f: A => B)(using F: Functor[F]): Free[F, B] = this match
    case Pure(a)     => Pure(f(a))
    case Suspend(fa) => Suspend(F.map(fa)(_.map(f)))
  
  def flatMap[B](f: A => Free[F, B])(using F: Functor[F]): Free[F, B] = this match
    case Pure(a)     => f(a)
    case Suspend(fa) => Suspend(F.map(fa)(_.flatMap(f)))
  
  /** Interpret into any monad M that has a natural transformation F ~> M */
  def foldMap[M[_]](nat: [X] => F[X] => M[X])(using M: Monad[M], F: Functor[F]): M[A] = 
    this match
      case Pure(a)     => M.pure(a)
      case Suspend(fa) => M.flatMap(nat(fa))(_.foldMap(nat))

object Free:
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)
  def liftF[F[_], A](fa: F[A])(using F: Functor[F]): Free[F, A] = 
    Suspend(F.map(fa)(Pure(_)))

/** Functor typeclass */
trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

/** Monad typeclass */
trait Monad[M[_]] extends Functor[M]:
  def pure[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))

// =============================================================================
// 2. Cofree Comonad - Annotated/attributed terms
// =============================================================================

/**
 * Cofree[F, A] is the cofree comonad over functor F.
 * It's a tree where every node is annotated with an A.
 * 
 * Key insight: Cofree[F, A] ≅ A × F[Cofree[F, A]]
 * - Each node has a value (the annotation)
 * - And children determined by F
 * 
 * Use case: ASTs with type annotations at every node
 */
case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]]):
  /** Extract the annotation at root */
  def extract: A = head
  
  /** Extend a function over all positions */
  def extend[B](f: Cofree[F, A] => B)(using F: Functor[F]): Cofree[F, B] =
    Cofree(f(this), F.map(tail)(_.extend(f)))
  
  /** Map over annotations */
  def map[B](f: A => B)(using F: Functor[F]): Cofree[F, B] =
    Cofree(f(head), F.map(tail)(_.map(f)))
  
  /** Duplicate - put the whole subtree at each position */
  def duplicate(using F: Functor[F]): Cofree[F, Cofree[F, A]] =
    Cofree(this, F.map(tail)(_.duplicate))

object Cofree:
  /** Build from a seed using a coalgebra */
  def unfold[F[_], A, S](seed: S)(coalg: S => (A, F[S]))(using F: Functor[F]): Cofree[F, A] =
    val (a, fs) = coalg(seed)
    Cofree(a, F.map(fs)(unfold(_)(coalg)))
  
  /** Annotate a Val with computed attributes at each node */
  def annotate[A](v: Val)(compute: Val => A): Cofree[ValF, A] =
    given Functor[ValF] = ValFFunctor
    Cofree(compute(v), RecursionSchemes.project(v) match
      case RecursionSchemes.ValF.ConF(n, args) => 
        RecursionSchemes.ValF.ConF(n, args.map(annotate(_)(compute)))
      case RecursionSchemes.ValF.StrF(s)       => RecursionSchemes.ValF.StrF(s)
      case RecursionSchemes.ValF.IntF(n)       => RecursionSchemes.ValF.IntF(n)
      case RecursionSchemes.ValF.ListF(elems)  => 
        RecursionSchemes.ValF.ListF(elems.map(annotate(_)(compute)))
    )

/** ValF functor instance */
given ValFFunctor: Functor[RecursionSchemes.ValF] with
  def map[A, B](fa: RecursionSchemes.ValF[A])(f: A => B): RecursionSchemes.ValF[B] =
    RecursionSchemes.mapF(fa)(f)

// Alias for convenience
type ValF[A] = RecursionSchemes.ValF[A]

// =============================================================================
// 3. Day Convolution - Combining independent computations
// =============================================================================

/**
 * Day[F, G, A] represents combining F and G computations.
 * It's the "tensor product" of functors.
 * 
 * Day[F, G, A] = ∃B C. F[B] × G[C] × (B × C → A)
 * 
 * Use case: Combining two parsers that can run independently
 */
case class Day[F[_], G[_], A, B, C](
  fb: F[B],
  gc: G[C],
  combine: (B, C) => A
):
  def map[D](f: A => D): Day[F, G, D, B, C] =
    Day(fb, gc, (b, c) => f(combine(b, c)))
  
  /** Run both and combine */
  def run(using F: Functor[F], G: Functor[G]): F[G[A]] =
    F.map(fb)(b => G.map(gc)(c => combine(b, c)))

object Day:
  /** Introduce Day from a pair */
  def apply[F[_], G[_], A, B](fa: F[A], gb: G[B]): Day[F, G, (A, B), A, B] =
    Day(fa, gb, (a, b) => (a, b))
  
  /** Day convolution for Syntax - parse two things independently */
  def syntax[A, B](sa: phi.meta.Syntax[A], sb: phi.meta.Syntax[B]): Day[
    [X] =>> phi.meta.Syntax[X], 
    [X] =>> phi.meta.Syntax[X], 
    (A, B), A, B
  ] = Day(sa, sb, (a, b) => (a, b))

// =============================================================================
// 4. Kan Extensions - Universal constructions
// =============================================================================

/**
 * Right Kan Extension: Ran[G, H, A] = ∀B. (A → G[B]) → H[B]
 * 
 * The "best approximation" of H using G.
 * Captures the essence of "continuation-passing style".
 */
trait Ran[G[_], H[_], A]:
  def apply[B](f: A => G[B]): H[B]

object Ran:
  /** Ran[Id, F, A] ≅ F[A] - right Kan extension along identity */
  def toFunctor[F[_], A](ran: Ran[[X] =>> X, F, A]): F[A] =
    ran(identity)
  
  /** F[A] → Ran[Id, F, A] */
  def fromFunctor[F[_], A](fa: F[A])(using F: Functor[F]): Ran[[X] =>> X, F, A] =
    new Ran[[X] =>> X, F, A]:
      def apply[B](f: A => B): F[B] = F.map(fa)(f)

/**
 * Left Kan Extension: Lan[G, H, A] = ∃B. G[B] × (B → H[A])
 * 
 * The "free completion" of H by G.
 * Captures the essence of "direct style with explicit continuations".
 */
case class Lan[G[_], H[_], A, B](gb: G[B], f: B => H[A]):
  def map[C](g: A => C)(using H: Functor[H]): Lan[G, H, C, B] =
    Lan(gb, b => H.map(f(b))(g))

object Lan:
  /** Lan[Id, F, A] ≅ F[A] - left Kan extension along identity */
  def toLan[F[_], A](fa: F[A]): Lan[[X] =>> X, F, A, F[A]] =
    Lan[[X] =>> X, F, A, F[A]](fa, fa => fa)

// =============================================================================
// 5. Yoneda - Optimization via CPS
// =============================================================================

/**
 * Yoneda[F, A] ≅ F[A] but with fused maps.
 * 
 * Multiple maps fuse into a single function composition,
 * avoiding intermediate structures.
 */
trait Yoneda[F[_], A]:
  def apply[B](f: A => B): F[B]
  
  def map[B](g: A => B): Yoneda[F, B] = new Yoneda[F, B]:
    def apply[C](f: B => C): F[C] = Yoneda.this.apply(f compose g)
  
  def lower(using F: Functor[F]): F[A] = apply(identity)

object Yoneda:
  def lift[F[_], A](fa: F[A])(using F: Functor[F]): Yoneda[F, A] = 
    new Yoneda[F, A]:
      def apply[B](f: A => B): F[B] = F.map(fa)(f)
  
  /** Optimization: many maps fuse into one */
  def fusedMaps[F[_], A](fa: F[A])(using F: Functor[F]): Yoneda[F, A] = lift(fa)

// =============================================================================
// 6. Coyoneda - Free functor
// =============================================================================

/**
 * Coyoneda[F, A] makes any type constructor F into a Functor.
 * 
 * Coyoneda[F, A] = ∃B. F[B] × (B → A)
 * 
 * Use case: Add functor operations to types that aren't functors
 */
case class Coyoneda[F[_], A, B](fb: F[B], f: B => A):
  def map[C](g: A => C): Coyoneda[F, C, B] = Coyoneda(fb, g compose f)
  
  def lower(using F: Functor[F]): F[A] = F.map(fb)(f)

object Coyoneda:
  def lift[F[_], A](fa: F[A]): Coyoneda[F, A, A] = Coyoneda(fa, identity)

// =============================================================================
// 7. Fix point - Explicit recursion
// =============================================================================

/**
 * Fix[F] is the fixed point of functor F.
 * Fix[F] ≅ F[Fix[F]]
 * 
 * This is the mathematical foundation for recursive data types.
 */
case class Fix[F[_]](unfix: F[Fix[F]]):
  def cata[A](alg: F[A] => A)(using F: Functor[F]): A =
    alg(F.map(unfix)(_.cata(alg)))
  
  def para[A](alg: F[(Fix[F], A)] => A)(using F: Functor[F]): A =
    alg(F.map(unfix)(fix => (fix, fix.para(alg))))

object Fix:
  def ana[F[_], A](coalg: A => F[A])(seed: A)(using F: Functor[F]): Fix[F] =
    Fix(F.map(coalg(seed))(ana(coalg)))
  
  def hylo[F[_], A, B](alg: F[B] => B)(coalg: A => F[A])(seed: A)(using F: Functor[F]): B =
    alg(F.map(coalg(seed))(hylo(alg)(coalg)))

// =============================================================================
// 8. Natural Transformations
// =============================================================================

/**
 * Natural transformation F ~> G is a polymorphic function.
 * For all A: F[A] → G[A], satisfying naturality.
 */
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

object `~>`:
  /** Identity natural transformation */
  def id[F[_]]: F ~> F = new (F ~> F):
    def apply[A](fa: F[A]): F[A] = fa
  
  /** Compose natural transformations */
  def compose[F[_], G[_], H[_]](fg: F ~> G, gh: G ~> H): F ~> H = 
    new (F ~> H):
      def apply[A](fa: F[A]): H[A] = gh(fg(fa))

// =============================================================================
// 9. Application: DSL for Term Operations
// =============================================================================

/**
 * Operations on terms as a Free monad DSL.
 * This makes the structure of term transformations explicit and inspectable.
 */
enum TermOp[A]:
  case GetNode(path: List[Int], k: Val => A)
  case SetNode(path: List[Int], value: Val, k: Unit => A)
  case Transform(xformName: String, k: Val => A)
  case Validate(k: List[String] => A)

object TermOp:
  given Functor[TermOp] with
    def map[A, B](fa: TermOp[A])(f: A => B): TermOp[B] = fa match
      case GetNode(p, k)      => GetNode(p, v => f(k(v)))
      case SetNode(p, v, k)   => SetNode(p, v, u => f(k(u)))
      case Transform(x, k)    => Transform(x, v => f(k(v)))
      case Validate(k)        => Validate(es => f(k(es)))
  
  // Smart constructors
  def getNode(path: List[Int]): Free[TermOp, Val] = 
    Free.liftF(GetNode(path, identity))
  
  def setNode(path: List[Int], value: Val): Free[TermOp, Unit] = 
    Free.liftF(SetNode(path, value, identity))
  
  def transform(xformName: String): Free[TermOp, Val] = 
    Free.liftF(Transform(xformName, identity))
  
  def validate: Free[TermOp, List[String]] = 
    Free.liftF(Validate(identity))
  
  // Example DSL program
  def exampleProgram: Free[TermOp, Val] =
    for
      root <- getNode(Nil)
      _    <- setNode(List(0), VStr("modified"))
      errs <- validate
      result <- if errs.isEmpty then transform("Greeting2Scala") 
                else Free.pure(VCon("Error", errs.map(VStr(_))))
    yield result

// =============================================================================
// 10. Instances for Term
// =============================================================================

given TermMonadInstance: Monad[Term] with
  def pure[A](a: A): Term[A] = Done(a)
  def flatMap[A, B](ma: Term[A])(f: A => Term[B]): Term[B] = ma match
    case Done(a) => f(a)
    case Hole(l) => Hole(l)

// =============================================================================
// Usage Examples
// =============================================================================

object AdvancedExamples:
  import RecursionSchemes.ValF.*
  
  // Example 1: Cofree for type-annotated AST
  def typeAnnotate(v: Val): Cofree[ValF, String] =
    given Functor[ValF] = ValFFunctor
    Cofree.annotate(v) {
      case VCon("Hello", _)      => "Greeting"
      case VCon("SimpleName", _) => "Name"
      case VStr(_)               => "String"
      case VInt(_)               => "Int"
      case VList(_)              => "List"
      case _                     => "Unknown"
    }
  
  // Example 2: Yoneda for fused maps
  def efficientPipeline[F[_]: Functor](fa: F[Int]): F[String] =
    Yoneda.lift(fa)
      .map(_ + 1)
      .map(_ * 2)
      .map(_.toString)
      .map("Result: " + _)
      .lower  // Only ONE actual map happens!
  
  // Example 3: Free monad for inspectable operations
  def runTermOp(term: Val, prog: Free[TermOp, Val]): Term[Val] =
    prog match
      case Free.Pure(v) => Done(v)
      case Free.Suspend(op) => op match
        case TermOp.GetNode(Nil, k) => runTermOp(term, k(term))
        case TermOp.GetNode(_, k)   => Hole(Some("path access not impl"))
        case TermOp.SetNode(_, _, k) => runTermOp(term, k(()))
        case TermOp.Transform(_, k) => runTermOp(term, k(term))
        case TermOp.Validate(k)     => runTermOp(term, k(Nil))
