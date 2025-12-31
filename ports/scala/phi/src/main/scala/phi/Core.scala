package phi

/**
 * ╔═══════════════════════════════════════════════════════════════════════════╗
 * ║                     Φ-CORE: Algebraic Foundation                          ║
 * ╠═══════════════════════════════════════════════════════════════════════════╣
 * ║  The complete algebraic infrastructure for metaprogramming in ~500 lines  ║
 * ╚═══════════════════════════════════════════════════════════════════════════╝
 *
 * PHILOSOPHY
 * ==========
 * 
 * Everything is a tree with annotations. This single insight unifies:
 *   - Zippers     = trees + locations
 *   - Attributes  = trees + computed values  
 *   - Versions    = trees + content hashes
 * 
 * All three are instances of Cofree[V, A] where A is the annotation type.
 *
 * ARCHITECTURE
 * ============
 * 
 * From just 4 type classes and 4 recursive structures, everything follows:
 *
 *   TYPE CLASSES (interfaces)           STRUCTURES (data)
 *   ┌──────────────────────────┐        ┌──────────────────────────┐
 *   │ F[_]    Functor          │        │ V[A]     Pattern functor │
 *   │ M[_]    Monad            │        │ μ[F]     Fix point       │
 *   │ W[_]    Comonad          │        │ Free[F,A] Free monad     │
 *   │ ~>      Natural trans    │        │ Co[F,A]   Cofree comonad │
 *   └──────────────────────────┘        └──────────────────────────┘
 *
 * QUICK START
 * ===========
 *
 *   import phi.Core.*
 *   
 *   // Fold a tree bottom-up
 *   val size = cata { 
 *     case C(_, args) => 1 + args.sum
 *     case _ => 1 
 *   }(ast)
 *   
 *   // Navigate with a zipper
 *   val modified = Zipper.from(ast)
 *     .navigate(List(0, 1))
 *     .modify(_ => newValue)
 *     .toVal
 *   
 *   // Parallel validation
 *   val result = (checkName zip checkType zip checkBody).map(...)
 */
object Core:

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 1: Values (The Universal AST)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Val: The universal value type for Phi programs.
   * 
   * Everything in Phi is represented as a Val:
   *   - AST nodes: VCon("Add", List(VCon("Lit", List(VInt(1))), ...))
   *   - Literals:  VStr("hello"), VInt(42)
   *   - Lists:     VList(List(VInt(1), VInt(2), VInt(3)))
   * 
   * This uniformity enables generic operations on any language.
   */
  enum Val:
    case VCon(name: String, args: List[Val])  // Constructor: Add(x, y)
    case VStr(s: String)                       // String literal
    case VInt(n: Int)                          // Integer literal
    case VList(elems: List[Val])               // List value
    
    /** Pretty-print this value */
    def show: String = this match
      case VCon(n, Nil)  => n
      case VCon(n, args) => s"$n(${args.map(_.show).mkString(", ")})"
      case VStr(s)       => s"\"$s\""
      case VInt(n)       => n.toString
      case VList(Nil)    => "[]"
      case VList(elems)  => s"[${elems.map(_.show).mkString(", ")}]"
    
    /** Get children for traversal */
    def children: List[Val] = this match
      case VCon(_, args) => args
      case VList(elems)  => elems
      case _             => Nil
    
    /** Map over immediate children */
    def mapChildren(f: Val => Val): Val = this match
      case VCon(n, args) => VCon(n, args.map(f))
      case VList(elems)  => VList(elems.map(f))
      case other         => other

  export Val.*

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 2: Type Classes (The Four Abstractions)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Functor: Transform contents while preserving structure.
   * 
   * Laws:
   *   map(fa)(identity) == fa              -- identity
   *   map(fa)(f andThen g) == map(map(fa)(f))(g)  -- composition
   * 
   * Examples:
   *   - List is a Functor: List(1,2,3).map(_ + 1) = List(2,3,4)
   *   - Option is a Functor: Some(1).map(_ + 1) = Some(2)
   *   - V is a Functor: we can map over AST children
   */
  trait Functor[F[_]]:
    def map[A, B](fa: F[A])(f: A => B): F[B]
    
    extension [A](fa: F[A])
      def fmap[B](f: A => B): F[B] = map(fa)(f)

  /**
   * Monad: Sequence dependent computations.
   * 
   * Laws:
   *   pure(a).flatMap(f) == f(a)           -- left identity
   *   m.flatMap(pure) == m                 -- right identity  
   *   m.flatMap(f).flatMap(g) == m.flatMap(a => f(a).flatMap(g))  -- associativity
   * 
   * Examples:
   *   - Option: chain operations that might fail
   *   - List: chain operations that produce multiple results
   *   - Free: chain operations as inspectable data
   */
  trait Monad[M[_]] extends Functor[M]:
    def pure[A](a: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    
    override def map[A, B](ma: M[A])(f: A => B): M[B] = 
      flatMap(ma)(a => pure(f(a)))
    
    extension [A](ma: M[A])
      def >>=[B](f: A => M[B]): M[B] = flatMap(ma)(f)

  /**
   * Comonad: Context-dependent computations.
   * 
   * Laws:
   *   extract(extend(wa)(f)) == f(wa)      -- left identity
   *   extend(wa)(extract) == wa            -- right identity
   *   extend(extend(wa)(f))(g) == extend(wa)(w => g(extend(w)(f)))  -- associativity
   * 
   * Intuition:
   *   - extract: get the focus from the context
   *   - extend: apply a context-dependent function everywhere
   * 
   * Examples:
   *   - Zipper: extract gets current node, extend annotates all nodes
   *   - Stream: extract gets head, extend makes stream of running computations
   */
  trait Comonad[W[_]] extends Functor[W]:
    def extract[A](wa: W[A]): A
    def extend[A, B](wa: W[A])(f: W[A] => B): W[B]
    
    override def map[A, B](wa: W[A])(f: A => B): W[B] = 
      extend(wa)(w => f(extract(w)))

  /**
   * Natural Transformation: Polymorphic function between functors.
   * 
   * For all types A: F[A] => G[A], preserving structure.
   * 
   * Example: List ~> Option (headOption)
   *   List(1,2,3) => Some(1)
   *   List() => None
   */
  trait ~>[F[_], G[_]]:
    def apply[A](fa: F[A]): G[A]

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 3: Pattern Functor V (One Layer of AST)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * V: The pattern functor - represents ONE layer of Val.
   * 
   * Conceptually: Val ≅ V[Val] (Val is the fixed point of V)
   * 
   * This factoring enables:
   *   - Generic recursion schemes (cata, ana, hylo)
   *   - Layer-by-layer processing
   *   - Separation of recursion from business logic
   */
  enum V[+A]:
    case C(name: String, args: List[A])  // Constructor node
    case S(value: String)                 // String leaf
    case I(value: Int)                    // Int leaf
    case L(elements: List[A])             // List node

  object V:
    /** V is a Functor - we can map over children */
    given Functor[V] with
      def map[A, B](v: V[A])(f: A => B): V[B] = v match
        case C(n, args) => C(n, args.map(f))
        case S(s)       => S(s)
        case I(i)       => I(i)
        case L(elems)   => L(elems.map(f))
    
    /** Project: unfold Val into V[Val] */
    def out(v: Val): V[Val] = v match
      case VCon(n, args) => C(n, args)
      case VStr(s)       => S(s)
      case VInt(i)       => I(i)
      case VList(elems)  => L(elems)
    
    /** Embed: fold V[Val] into Val */
    def in(v: V[Val]): Val = v match
      case C(n, args) => VCon(n, args)
      case S(s)       => VStr(s)
      case I(i)       => VInt(i)
      case L(elems)   => VList(elems)

  export V.{C, S, I, L}

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 4: Recursion Schemes (Generic Tree Operations)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Catamorphism: Generic fold (bottom-up).
   * 
   * Given an algebra (V[A] => A), collapse a tree to a single value.
   * 
   * Example - count nodes:
   *   cata {
   *     case C(_, args) => 1 + args.sum
   *     case _ => 1
   *   }(ast)
   */
  def cata[A](algebra: V[A] => A)(v: Val): A =
    algebra(summon[Functor[V]].map(V.out(v))(cata(algebra)))

  /**
   * Anamorphism: Generic unfold (top-down).
   * 
   * Given a coalgebra (A => V[A]), grow a tree from a seed.
   * 
   * Example - generate list:
   *   ana { n => if n <= 0 then V.L(Nil) else V.C("S", List(n-1)) }(5)
   */
  def ana[A](coalgebra: A => V[A])(seed: A): Val =
    V.in(summon[Functor[V]].map(coalgebra(seed))(ana(coalgebra)))

  /**
   * Hylomorphism: Fused unfold-then-fold.
   * 
   * Build and immediately consume without materializing intermediate tree.
   * This is often more efficient than ana followed by cata.
   */
  def hylo[A, B](algebra: V[B] => B, coalgebra: A => V[A])(seed: A): B =
    algebra(summon[Functor[V]].map(coalgebra(seed))(hylo(algebra, coalgebra)))

  /**
   * Paramorphism: Fold with access to original subtrees.
   * 
   * Like cata but the algebra also sees the original Val at each step.
   * Useful when you need both the result AND the original structure.
   */
  def para[A](algebra: V[(Val, A)] => A)(v: Val): A =
    algebra(summon[Functor[V]].map(V.out(v))(child => (child, para(algebra)(child))))

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 5: Free Monad (Inspectable Effects)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Free: The free monad over a functor F.
   * 
   * Represents computations as DATA that can be:
   *   - Inspected before running
   *   - Run with different interpreters
   *   - Optimized/transformed
   * 
   * Structure:
   *   Pure(a)  - computation finished with value a
   *   Roll(fa) - computation needs effect fa to continue
   */
  enum Free[F[_], A]:
    case Pure(value: A)
    case Roll(step: F[Free[F, A]])

  object Free:
    /** Lift an effect into Free */
    def liftF[F[_]: Functor, A](fa: F[A]): Free[F, A] = 
      Roll(summon[Functor[F]].map(fa)(Pure(_)))
    
    /** Free[F, _] is a Monad when F is a Functor */
    given [F[_]: Functor]: Monad[[A] =>> Free[F, A]] with
      def pure[A](a: A): Free[F, A] = Pure(a)
      
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = 
        fa match
          case Pure(a)  => f(a)
          case Roll(ff) => Roll(summon[Functor[F]].map(ff)(flatMap(_)(f)))
    
    /**
     * Interpret Free into any monad via natural transformation.
     * 
     * This is Free's superpower: write your program once as data,
     * then run it with different interpreters for testing, logging, etc.
     */
    def foldMap[F[_]: Functor, M[_]: Monad, A](fa: Free[F, A])(nt: F ~> M): M[A] =
      fa match
        case Pure(a)  => summon[Monad[M]].pure(a)
        case Roll(ff) => summon[Monad[M]].flatMap(nt(ff))(foldMap(_)(nt))

  export Free.{Pure, Roll}

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 6: Cofree Comonad (Annotated Trees)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Cofree: The cofree comonad over a functor F.
   * 
   * A tree where EVERY node carries an annotation:
   *   - head: the annotation at this node
   *   - tail: the children (wrapped in F)
   * 
   * This is the KEY insight: Cofree unifies zippers, attributes, and versions!
   * 
   *   Zipper     = Cofree[V, (Val, Path)]   -- tree + locations
   *   Attributed = Cofree[V, Attr]          -- tree + computed values
   *   Versioned  = Cofree[V, Hash]          -- tree + content hashes
   */
  case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])

  object Cofree:
    /** Cofree[F, _] is a Comonad when F is a Functor */
    given [F[_]: Functor]: Comonad[[A] =>> Cofree[F, A]] with
      def extract[A](wa: Cofree[F, A]): A = wa.head
      
      def extend[A, B](wa: Cofree[F, A])(f: Cofree[F, A] => B): Cofree[F, B] =
        Cofree(f(wa), summon[Functor[F]].map(wa.tail)(extend(_)(f)))
    
    /**
     * Universal attribute grammar in ONE function.
     * 
     * This single function implements ALL attribute grammars:
     *   - inherit: compute inherited attrs (parent → children)
     *   - synth: compute synthesized attrs (children → parent)
     * 
     * Both directions happen in ONE pass! The magic:
     * Haskell's laziness / Scala's by-name parameters let us
     * reference "future" values that will be filled in.
     */
    def annotate[I, S](v: Val)(
      inherit: (Val, I) => I,
      synth: (Val, I, List[S]) => S,
      init: I
    ): Cofree[V, S] =
      def go(v: Val, inh: I): Cofree[V, S] =
        val childInh = inherit(v, inh)
        val structure = V.out(v)
        val annotated = structure match
          case V.C(_, args) => args.map(go(_, childInh))
          case V.L(elems)   => elems.map(go(_, childInh))
          case _            => Nil
        val syn = synth(v, childInh, annotated.map(_.head))
        val newTail = structure match
          case V.C(n, _) => V.C(n, annotated)
          case V.L(_)    => V.L(annotated)
          case V.S(s)    => V.S(s)
          case V.I(i)    => V.I(i)
        Cofree(syn, newTail)
      go(v, init)
    
    /** Strip annotations to recover original Val */
    def forget[A](cf: Cofree[V, A]): Val =
      V.in(summon[Functor[V]].map(cf.tail)(forget))

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 7: Optics (Lens, Prism, Traversal)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Lens: Focus on exactly ONE part of a structure.
   * 
   * Laws:
   *   get(set(a)(s)) == a           -- you get what you set
   *   set(get(s))(s) == s           -- setting what's there changes nothing
   *   set(a)(set(b)(s)) == set(a)(s)  -- set twice = set once
   */
  case class Lens[S, A](get: S => A, set: A => S => S):
    def modify(f: A => A)(s: S): S = set(f(get(s)))(s)
    def andThen[B](other: Lens[A, B]): Lens[S, B] = Lens(
      s => other.get(get(s)),
      b => s => set(other.set(b)(get(s)))(s)
    )

  /**
   * Prism: Focus on ONE CASE of a sum type.
   * 
   * Like a Lens, but the focus may not exist (for other cases).
   * 
   * Example: Prism focusing on VCon with name "Add"
   */
  case class Prism[S, A](getOption: S => Option[A], reverseGet: A => S):
    def modify(f: A => A)(s: S): S = 
      getOption(s).map(a => reverseGet(f(a))).getOrElse(s)

  /**
   * Traversal: Focus on ZERO OR MORE parts.
   * 
   * Generalizes Lens (exactly 1) and Prism (0 or 1).
   */
  trait Traversal[S, A]:
    def getAll(s: S): List[A]
    def modify(f: A => A)(s: S): S

  // Common optics for Val
  object Optics:
    /** Lens into nth argument of a constructor */
    def arg(n: Int): Lens[Val, Val] = Lens(
      { case VCon(_, args) => args(n); case v => v },
      newArg => { case VCon(name, args) => VCon(name, args.updated(n, newArg)); case s => s }
    )
    
    /** Prism matching constructor by name */
    def con(name: String): Prism[Val, List[Val]] = Prism(
      { case VCon(n, args) if n == name => Some(args); case _ => None },
      args => VCon(name, args)
    )
    
    /** Traversal over all immediate children */
    val children: Traversal[Val, Val] = new Traversal:
      def getAll(s: Val) = s.children
      def modify(f: Val => Val)(s: Val) = s.mapChildren(f)
    
    /** Transform everywhere in tree (bottom-up) */
    def everywhere(f: Val => Val)(v: Val): Val = 
      f(v.mapChildren(everywhere(f)))
    
    /** Transform everywhere (top-down) */
    def everywhereDown(f: Val => Val)(v: Val): Val =
      f(v).mapChildren(everywhereDown(f))

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 8: Validated (Parallel Error Accumulation)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Validated: Like Either, but accumulates ALL errors.
   * 
   * Unlike Either which short-circuits on first error,
   * Validated collects errors via zip for parallel validation.
   * 
   * Example:
   *   val name = validateName(input)    // Validated[List[String], Name]
   *   val age = validateAge(input)      // Validated[List[String], Int]
   *   val person = (name zip age).map(Person.apply)  // gets ALL errors!
   */
  enum Validated[+E, +A]:
    case Valid(value: A)
    case Invalid(errors: List[E])
    
    def map[B](f: A => B): Validated[E, B] = this match
      case Valid(a)      => Valid(f(a))
      case Invalid(errs) => Invalid(errs)
    
    /** Combine validations, accumulating errors */
    def zip[E2 >: E, B](other: Validated[E2, B]): Validated[E2, (A, B)] = 
      (this, other) match
        case (Valid(a), Valid(b))       => Valid((a, b))
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)  // Both errors!
        case (Invalid(e), _)            => Invalid(e)
        case (_, Invalid(e))            => Invalid(e)
    
    def toEither: Either[List[E], A] = this match
      case Valid(a)      => Right(a)
      case Invalid(errs) => Left(errs)

  object Validated:
    def valid[A](a: A): Validated[Nothing, A] = Valid(a)
    def invalid[E](e: E): Validated[E, Nothing] = Invalid(List(e))
    def invalid[E](es: List[E]): Validated[E, Nothing] = Invalid(es)
    
    /** Sequence a list of validations */
    def sequence[E, A](vs: List[Validated[E, A]]): Validated[E, List[A]] =
      vs.foldRight(valid(List.empty[A])) { (v, acc) =>
        v.zip(acc).map(_ :: _)
      }

  export Validated.{Valid, Invalid}

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 9: Zipper (Navigation with Context)
  // ═══════════════════════════════════════════════════════════════════════════

  /** Location in a tree: current value + path from root */
  case class Loc(value: Val, path: List[Int])

  /** Zipper: Cofree annotated with locations */
  type Zipper = Cofree[V, Loc]

  object Zipper:
    /** Create a zipper from a Val */
    def from(v: Val): Zipper =
      def go(v: Val, path: List[Int]): Zipper =
        val structure = V.out(v)
        val annotated = structure match
          case V.C(n, args) => V.C(n, args.zipWithIndex.map((c, i) => go(c, path :+ i)))
          case V.L(elems)   => V.L(elems.zipWithIndex.map((c, i) => go(c, path :+ i)))
          case V.S(s)       => V.S(s)
          case V.I(i)       => V.I(i)
        Cofree(Loc(v, path), annotated)
      go(v, Nil)
    
    /** Navigate to a path, returning None if invalid */
    def navigate(z: Zipper, path: List[Int]): Option[Zipper] = path match
      case Nil => Some(z)
      case i :: rest => z.tail match
        case V.C(_, args) if args.length > i => navigate(args(i), rest)
        case V.L(elems) if elems.length > i  => navigate(elems(i), rest)
        case _ => None
    
    /** Modify the value at current focus */
    def modify(z: Zipper)(f: Val => Val): Zipper = 
      Cofree(Loc(f(z.head.value), z.head.path), z.tail)
    
    /** Convert back to Val (strips annotations) */
    def toVal(z: Zipper): Val = Cofree.forget(z)

    extension (z: Zipper)
      def go(path: List[Int]): Option[Zipper] = navigate(z, path)
      def update(f: Val => Val): Zipper = modify(z)(f)
      def value: Val = z.head.value
      def currentPath: List[Int] = z.head.path

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 10: Xform (Bidirectional Transforms)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Xform: Bidirectional transformation A ⇄ B.
   * 
   * Unlike a simple function A => B, captures BOTH directions.
   * Enables round-tripping and synchronization.
   */
  trait Xform[A, B]:
    def forward(a: A): Option[B]
    def backward(b: B): Option[A]
    
    def andThen[C](other: Xform[B, C]): Xform[A, C] = Xform.compose(this, other)
    def inverse: Xform[B, A] = Xform.invert(this)

  object Xform:
    def apply[A, B](fwd: A => Option[B], bwd: B => Option[A]): Xform[A, B] =
      new Xform[A, B]:
        def forward(a: A) = fwd(a)
        def backward(b: B) = bwd(b)
    
    def id[A]: Xform[A, A] = Xform(Some(_), Some(_))
    
    def compose[A, B, C](f: Xform[A, B], g: Xform[B, C]): Xform[A, C] =
      Xform(
        a => f.forward(a).flatMap(g.forward),
        c => g.backward(c).flatMap(f.backward)
      )
    
    def invert[A, B](x: Xform[A, B]): Xform[B, A] =
      Xform(x.backward, x.forward)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 11: Hash & Store (Content Addressing)
  // ═══════════════════════════════════════════════════════════════════════════

  /** Content hash (opaque type for type safety) */
  opaque type Hash = String
  
  object Hash:
    def apply(s: String): Hash = s
    def of(v: Val): Hash = v.show.hashCode.toHexString.take(8)
    extension (h: Hash) def value: String = h

  /**
   * Content-addressed store: deduplicates identical values.
   * 
   * Same value → same hash. Enables:
   *   - Structural sharing
   *   - O(1) equality via hash comparison
   *   - Caching of computed results
   */
  class Store[A](hash: A => Hash):
    private var storage: Map[Hash, A] = Map.empty
    
    def put(a: A): Hash =
      val h = hash(a)
      if !storage.contains(h) then storage = storage + (h -> a)
      h
    
    def get(h: Hash): Option[A] = storage.get(h)
    def size: Int = storage.size

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 12: Yoneda (Map Fusion Optimization)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Yoneda lemma: Yoneda[F, A] ≅ F[A]
   * 
   * The trick: multiple maps FUSE into a single traversal.
   * 
   * Instead of: fa.map(f).map(g).map(h)  -- 3 traversals
   * Use:        Yoneda.lift(fa).map(f).map(g).map(h).lower  -- 1 traversal!
   */
  trait Yoneda[F[_], A]:
    def apply[B](f: A => B): F[B]
    
    def map[B](g: A => B): Yoneda[F, B] = new Yoneda[F, B]:
      def apply[C](h: B => C): F[C] = Yoneda.this.apply(g andThen h)
    
    def lower(using F: Functor[F]): F[A] = apply(identity)

  object Yoneda:
    def lift[F[_]: Functor, A](fa: F[A]): Yoneda[F, A] = new Yoneda[F, A]:
      def apply[B](f: A => B): F[B] = summon[Functor[F]].map(fa)(f)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 13: Common Instances
  // ═══════════════════════════════════════════════════════════════════════════

  /** Option is a Monad */
  given Monad[Option] with
    def pure[A](a: A) = Some(a)
    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma.flatMap(f)

  /** List is a Monad */
  given Monad[List] with
    def pure[A](a: A) = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma.flatMap(f)

  /** Either is a Monad (in the second type parameter) */
  given [E]: Monad[[A] =>> Either[E, A]] with
    def pure[A](a: A) = Right(a)
    def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]) = ma.flatMap(f)

  // ═══════════════════════════════════════════════════════════════════════════
  // SECTION 14: Convenience API (Φ)
  // ═══════════════════════════════════════════════════════════════════════════

  /**
   * Φ: Unified entry point for Core operations.
   * 
   * Usage:
   *   import phi.Core.Φ.*
   *   
   *   val counted = cata[Int] { case C(_, args) => 1 + args.sum; case _ => 1 }(ast)
   *   val zipped = zip(ast).go(List(0, 1)).map(_.value)
   */
  object Φ:
    // Recursion schemes
    export Core.{cata, ana, hylo, para}
    
    // Optics
    def arg(n: Int) = Optics.arg(n)
    def con(name: String) = Optics.con(name)
    def children = Optics.children
    def everywhere(f: Val => Val) = Optics.everywhere(f)
    
    // Zipper
    def zip(v: Val) = Zipper.from(v)
    
    // Validation
    def valid[A](a: A) = Validated.valid(a)
    def invalid[E](e: E) = Validated.invalid(e)
    
    // Hash
    def hash(v: Val) = Hash.of(v)

// End of Core object
