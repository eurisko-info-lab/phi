# Phi Mathematical Foundations

This document explains the mathematical structures underlying Phi and their
direct mapping to code. Understanding these concepts makes the system predictable
and compositional.

---

## 1. Signatures and Algebras

### Mathematical Definition

A **signature** Σ = (S, Ω) consists of:
- **S**: A set of *sorts* (types)
- **Ω**: A set of *operations* (constructors), each with an arity

An **algebra** A over Σ assigns:
- To each sort s ∈ S, a carrier set Aₛ
- To each operation ω: s₁ × ... × sₙ → s, a function Aω: As₁ × ... × Asₙ → As

### In Phi (.phi files)

```
language Hello {
  sort Greeting          -- S = {Greeting, Name}
  sort Name
  
  constructor Hello : Name → Greeting           -- Ω = {Hello, SimpleName}
  constructor SimpleName : String → Name
}
```

### In Scala (LangSpec.scala)

```scala
case class Sort(name: String)                           // s ∈ S
case class Constructor(
  name: String,                                         // ω
  argTypes: List[LangType],                             // s₁ × ... × sₙ
  returnType: String                                    // → s
)

case class LangSpec(
  name: String,
  sorts: List[Sort],                                    // S
  constructors: List[Constructor]                       // Ω
)
```

---

## 2. Terms (Free Algebra)

### Mathematical Definition

The **term algebra** T(Σ) is the *free algebra* over signature Σ:
- Elements are syntax trees built from constructors
- No equations (no junk, no confusion)
- Universal property: any algebra A has a unique homomorphism T(Σ) → A

```
T(Σ_hello) = { Hello(SimpleName("World")), 
               Hello(SimpleName("Phi")), 
               Hello(SimpleName("x")), ... }
```

### In Phi (runtime)

Terms are represented uniformly as `Val`:

```scala
enum Val:
  case VCon(name: String, args: List[Val])   // constructor application
  case VStr(s: String)                        // string literal
  case VInt(n: Int)                           // integer literal
  case VList(elems: List[Val])                // list
```

Example term:
```scala
VCon("Hello", List(
  VCon("SimpleName", List(
    VCon("String", List(VCon("World", Nil)))
  ))
))
```

### Term[A] with Holes

Real parsing may produce **partial terms**:

```scala
enum Term[+A]:
  case Done(value: A)        // complete term
  case Hole(label: Option[String])  // incomplete/unknown
```

This forms a **pointed functor** - every type A is extended with a distinguished
element ⊥ (the hole). Useful for:
- Incomplete parses
- Type inference placeholders
- Structured editor gaps

---

## 3. Homomorphisms (Transforms)

### Mathematical Definition

A **homomorphism** h: A → B between Σ-algebras preserves structure:

```
h(ω(a₁, ..., aₙ)) = ω'(h(a₁), ..., h(aₙ))
```

For each constructor in the source, there's a corresponding construction in the target.

### In Phi (.phi files)

```
xform Greeting2Scala : Greeting ⇄ ScalaStmt

rule Greeting2Scala.hello {
  Hello(name) ↦ PrintLn(StringConcat(
                  StringLit("Hi, "),
                  StringConcat(Name2Scala.forward(name), StringLit("!"))))
}

rule Name2Scala.simple {
  SimpleName(s) ↦ StringLit(s)
}
```

This defines:
- h(Hello(n)) = PrintLn(StringConcat(StringLit("Hi, "), StringConcat(h'(n), StringLit("!"))))
- h'(SimpleName(s)) = StringLit(s)

### In Scala (Xform.scala)

```scala
trait Xform[A, B]:
  def forward(term: Term[A]): Term[B]    // h: A → B
  def backward(term: Term[B]): Term[A]   // h⁻¹: B → A (when exists)
  
  def andThen[C](other: Xform[B, C]): Xform[A, C]  // composition
  def inverse: Xform[B, A]                          // flip direction
```

### Composition

Homomorphisms compose:

```
h: T(Σ₁) → T(Σ₂)
g: T(Σ₂) → T(Σ₃)
g ∘ h: T(Σ₁) → T(Σ₃)
```

In code:
```scala
val hello2scala: Xform[Val, Val] = ...
val scala2string: Xform[Val, String] = ...
val hello2string = hello2scala.andThen(scala2string)
```

---

## 4. Initial Algebras and Catamorphisms

### Mathematical Definition

The term algebra T(Σ) is the **initial algebra** in the category of Σ-algebras.
This means for any algebra A, there's a *unique* homomorphism:

```
⟦_⟧: T(Σ) → A
```

This unique map is called a **catamorphism** (fold).

### In Phi

Every transform rule set defines a catamorphism:

```
⟦Hello(n)⟧ = PrintLn(StringConcat(StringLit("Hi, "), ...))
⟦SimpleName(s)⟧ = StringLit(s)
```

The rules *are* the algebra structure on the target, and the transform
*is* the unique catamorphism from the initial algebra.

### Structural Recursion

The pattern in rule definitions follows structural recursion:

```
rule F.case {
  Con(x₁, ..., xₙ) ↦ result(F.forward(x₁), ..., F.forward(xₙ))
}
```

This guarantees:
- **Totality**: Every constructor has a rule
- **Termination**: Recursion follows term structure
- **Compositionality**: Each subterm transformed independently

---

## 5. Bidirectional Transforms (Isos)

### Mathematical Definition

An **isomorphism** is a pair of morphisms (f, g) where:
- f: A → B
- g: B → A  
- g ∘ f = id_A
- f ∘ g = id_B

### In Phi

```scala
case class Iso[A, B](
  forward: A => B,
  backward: B => A
)
```

Grammars define isomorphisms between strings and ASTs:

```
parse: String → Term[A]
render: Term[A] → String
render ∘ parse ≈ id  (round-trip)
```

### Validation

```scala
def roundTrip(term: Term[A]): Boolean =
  backward(forward(term)) == term  // f⁻¹(f(x)) = x
```

---

## 6. Attribute Grammars

### Mathematical Definition

An **attribute grammar** extends a CFG with:
- **Inherited attributes**: flow from parent to child (∀i)
- **Synthesized attributes**: flow from children to parent (∃)

Mathematically, these are:
- **Inherited**: Comonad-like (context flows down)
- **Synthesized**: Monad-like (values flow up)

### In Phi

```scala
enum FlowDirection:
  case Down   // inherited: ∀-like (available everywhere below)
  case Up     // synthesized: ∃-like (computed from below)
  case Both   // bidirectional
```

Example: Indentation is inherited
```scala
InheritedFlow("indent", (node, attrs) =>
  val current = attrs("indent").asInstanceOf[Int]
  Map("indent" -> (current + indentDelta(node)))
)
```

Example: Type is synthesized
```scala
SynthesizedFlow("type", (node, childTypes) =>
  inferType(node, childTypes)
)
```

---

## 7. Content-Addressed Storage (Hash Consing)

### Mathematical Definition

**Hash consing** implements a quotient:

```
Terms / ≡_structural  →  Hashes
```

Two terms with identical structure get the same hash.
This is a **canonical representative** for each equivalence class.

### In Phi

```scala
opaque type Hash = String

object Hash:
  def compute[A](term: Term[A]): Hash =
    SHA256(serialize(term)).take(16)
```

Properties:
- **Deterministic**: Same term → same hash
- **Collision-resistant**: Different terms → different hashes (w.h.p.)
- **Structural**: Hash depends only on structure, not identity

### Canonical Forms

```scala
class HashConsedStore[A]:
  def intern(term: Term[A]): (Hash, Term[A])  // get canonical
  def get(hash: Hash): Option[Term[A]]        // lookup by hash
```

This gives us:
- O(1) equality checking (compare hashes)
- Subterm sharing (same subterm = same hash)
- Content-addressed storage (lookup by content, not location)

---

## 8. Patches and Version Control

### Mathematical Definition

A **patch** p is an element of a group (P, ∘, ⁻¹, e):
- **Composition**: p₂ ∘ p₁ applies p₁ then p₂
- **Inverse**: p⁻¹ undoes p
- **Identity**: e is the no-op patch

Patches form a **groupoid** over states:
```
State₁ --p--> State₂ --q--> State₃
       <--p⁻¹--      <--q⁻¹--
```

### In Phi

```scala
case class Patch[A](
  id: Hash,
  change: Change[A],      // p
  inverse: Change[A],     // p⁻¹
  dependencies: Set[Hash] // which patches must be applied first
)

enum Change[A]:
  case Insert(value: A)           // fill hole
  case Replace(newTerm: Term[A])  // substitute
  case Delete                     // create hole
  case Sequence(changes: List[Change[A]])  // p₂ ∘ p₁
```

Inversion:
```scala
def invert(original: Term[A]): Change[A] = this match
  case Insert(_) => Delete
  case Replace(_) => Replace(original)
  case Delete => original match
    case Done(v) => Insert(v)
    case Hole(_) => Delete
```

---

## 9. Zippers (Derivatives)

### Mathematical Definition

The **zipper** for a type T is its **derivative** ∂T:
```
Zipper[T] ≅ T × ∂T
```

For algebraic data types, derivatives follow calculus rules:
- ∂(A + B) = ∂A + ∂B
- ∂(A × B) = (∂A × B) + (A × ∂B)
- ∂(μX.F(X)) = List[∂F] (for recursive types)

### In Phi

```scala
case class TermZipper[A](
  focus: Term[A],                    // current position
  context: List[ZipperContext[A]]    // path to root (∂T)
)

enum ZipperContext[A]:
  case Parent[A, B](
    parent: B,                       // sibling data
    rebuild: (B, Term[A]) => B       // reconstruction function
  )
```

Navigation:
```scala
def up: Option[TermZipper[?]]    // ∂T → T (integrate)
def down[B](...): Option[TermZipper[B]]  // T → ∂T (differentiate)
```

---

## 10. The Full Picture

### Category Theory View

```
                    Xform
        T(Σ_hello) --------> T(Σ_scala)
             |                    |
        show |                    | show
             ↓                    ↓
          String  <--------->  String
                    parse/render
```

Each arrow is a morphism. The diagram commutes:
```
render ∘ xform = xform_string ∘ render
```

### Algebraic Pipeline

```
Input String
     │
     │ parse (via grammar)
     ↓
Term[Source]  ←── holes possible
     │
     │ xform.forward (catamorphism)
     ↓
Term[Target]
     │
     │ render (via grammar)
     ↓
Output String
```

### Code Mapping Summary

| Math Concept | Phi Code | Purpose |
|--------------|----------|---------|
| Signature Σ | `LangSpec` | Define language structure |
| Sort s | `Sort` | Types in the language |
| Operation ω | `Constructor` | Ways to build terms |
| Term algebra T(Σ) | `Val.VCon` | AST representation |
| Partial term | `Term[A]` | With holes |
| Homomorphism h | `Xform[A,B]` | Structure-preserving map |
| Catamorphism | `rule` declarations | Fold over terms |
| Isomorphism | `Iso[A,B]` | Invertible transform |
| Initial algebra | Free `Val` construction | No equations |
| Attribute | `AttrDef` + flows | Computed properties |
| Canonical form | `Hash` | Content-addressed |
| Patch group | `Patch[A]` | Invertible edits |
| Derivative | `TermZipper` | Navigation context |
| Functor | `Term[A].map` | Lift functions |
| Monad | `Term[A].flatMap` | Sequenced computation |
| Comonad | `TermZipper.extend` | Context-aware transform |
| F-Algebra | `ValF[A] => A` | One layer of structure |
| Anamorphism | `ana` | Unfold from seed |
| Hylomorphism | `hylo` | Refold (fused unfold+fold) |
| Paramorphism | `para` | Fold with original access |
| Lens | `Lens[S,A]` | Focus exactly one |
| Prism | `Prism[S,A]` | Focus maybe one (match) |
| Traversal | `Traversal[S,A]` | Focus many |
| Profunctor | `Xform.dimap` | Adapt both sides |
| Applicative | `Validated.zip` | Parallel combination |

---

## 11. Additional Algebraic Structures

### Monad: Term[A]

A **monad** gives us sequenced computation with potential failure:

```
return : A → M[A]           -- pure/Done
(>>=) : M[A] → (A → M[B]) → M[B]  -- flatMap
```

```scala
// Term is a Monad
extension [A](term: Term[A])
  def flatMap[B](f: A => Term[B]): Term[B] = term match
    case Done(a) => f(a)
    case Hole(l) => Hole(l)

// Enables for-comprehensions
for
  parsed <- parse(input)      // Term[Val]
  xformed <- transform(parsed) // Term[Val]
  code <- render(xformed)     // Term[String]
yield code
```

**Laws** (automatically satisfied):
- Left identity: `Done(a).flatMap(f) = f(a)`
- Right identity: `m.flatMap(Done) = m`
- Associativity: `m.flatMap(f).flatMap(g) = m.flatMap(a => f(a).flatMap(g))`

### Comonad: TermZipper

A **comonad** gives us context-aware transformations:

```
extract : W[A] → A           -- get current focus
extend : (W[A] → B) → W[A] → W[B]  -- apply everywhere
```

```scala
extension [A](zipper: TermZipper[A])
  def extract: Term[A] = zipper.focus
  def extend[B](f: TermZipper[A] => B): TermZipper[B] = ...

// Compute depth at every position
zipper.extend(_.depth)

// Compute type at every position (with context!)
zipper.extend(inferType)
```

**Duality**: Monad sequences effects; Comonad distributes context.

### Recursion Schemes: Val

Recursion schemes factor out recursion patterns:

| Scheme | Type | Pattern |
|--------|------|---------|
| **cata** | `(F[A] → A) → Fix[F] → A` | Fold (consume) |
| **ana** | `(A → F[A]) → A → Fix[F]` | Unfold (produce) |
| **hylo** | `(F[B] → B) → (A → F[A]) → A → B` | Refold (fused) |
| **para** | `(F[(Fix[F], A)] → A) → Fix[F] → A` | Fold + original |

```scala
// Pattern functor for Val
enum ValF[+A]:
  case ConF(name: String, args: List[A])
  case StrF(s: String)
  case IntF(n: Int)
  case ListF(elems: List[A])

// Catamorphism
def cata[A](alg: ValF[A] => A)(v: Val): A =
  alg(project(v).map(cata(alg)))

// Count nodes (no explicit recursion!)
val count: Val => Int = cata {
  case ConF(_, args) => 1 + args.sum
  case StrF(_)       => 1
  case IntF(_)       => 1
  case ListF(elems)  => 1 + elems.sum
}
```

### Optics: Composable Access

Optics are first-class "getters and setters":

```
Lens[S, A]      -- focus on exactly one A in S
Prism[S, A]     -- focus on maybe one A in S (partial)
Traversal[S, A] -- focus on zero or more A's in S
```

```scala
trait Lens[S, A]:
  def get(s: S): A
  def set(a: A)(s: S): S
  def modify(f: A => A)(s: S): S = set(f(get(s)))(s)

// Composition!
val greetingName: Lens[Val, Val] = 
  constructor("Hello").andThen(arg(0)).andThen(constructor("SimpleName")).andThen(arg(0))

// Deep traversal
val allStrings: Traversal[Val, Val] = everywhere.filter(_.isInstanceOf[VStr])
```

### Profunctor: Xform

A **profunctor** allows mapping on both sides:

```
dimap : (A' → A) → (B → B') → P[A,B] → P[A',B']
```

```scala
// Xform is a Profunctor
def lmap[A0](f: A0 => A): Xform[A0, B]  // adapt input
def rmap[B0](g: B => B0): Xform[A, B0]  // adapt output
def dimap[A0, B0](f: A0 => A)(g: B => B0): Xform[A0, B0]

// Compose xform with arbitrary pre/post processing
val pipeline = xform.lmap(preprocess).rmap(postprocess)
```

### Applicative: Validated

Unlike Monad (sequential), **Applicative** allows parallel combination:

```scala
enum Validated[+E, +A]:
  case Valid(value: A)
  case Invalid(errors: List[E])
  
  def zip[E2 >: E, B](other: Validated[E2, B]): Validated[E2, (A, B)] = 
    (this, other) match
      case (Valid(a), Valid(b))       => Valid((a, b))
      case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)  // Collect BOTH!
      case (Invalid(e), _)            => Invalid(e)
      case (_, Invalid(e))            => Invalid(e)

// Collect all validation errors at once
Validated.map3(
  checkSyntax(term),
  checkTypes(term),
  checkNames(term)
)(combineResults)
```

---

## 12. Advanced Structures (Next Level)

### Free Monad: Explicit Effect Sequencing

The **Free Monad** over F makes effect sequencing inspectable:

```
Free[F, A] ≅ A + F[Free[F, A]]
```

```scala
enum Free[F[_], A]:
  case Pure(a: A)                    // Finished
  case Suspend(fa: F[Free[F, A]])    // One step, then continue

// Interpret into ANY monad with F ~> M
def foldMap[M[_]](nat: F ~> M): M[A]
```

**Use case**: Build a DSL of term operations, then choose interpreter:
```scala
enum TermOp[A]:
  case GetNode(path: List[Int], k: Val => A)
  case SetNode(path: List[Int], value: Val, k: Unit => A)
  case Transform(name: String, k: Val => A)

// Same program, different interpreters
val program: Free[TermOp, Val] = for
  node <- getNode(List(0))
  _    <- setNode(List(0), modified)
  out  <- transform("Greeting2Scala")
yield out

program.foldMap(realInterpreter)   // Actually run
program.foldMap(mockInterpreter)   // Test
program.foldMap(loggingInterpreter) // Debug
```

### Cofree Comonad: Annotated Terms

The **Cofree Comonad** over F is an F-branching tree with annotations:

```
Cofree[F, A] ≅ A × F[Cofree[F, A]]
```

```scala
case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]]):
  def extract: A                                    // Get annotation
  def extend[B](f: Cofree[F, A] => B): Cofree[F, B] // Apply everywhere
```

**Use case**: Type-annotated AST where every node has its type:
```scala
val typed: Cofree[ValF, Type] = Cofree.annotate(term)(inferType)
// Now every position has its computed type!
```

**Duality**:
- Free = build up effects (sequence)
- Cofree = tear down with context (annotate)

### Yoneda Lemma: Fused Maps

The **Yoneda lemma** says: `Yoneda[F, A] ≅ F[A]`

But Yoneda representation fuses multiple maps:

```scala
trait Yoneda[F[_], A]:
  def apply[B](f: A => B): F[B]
  def map[B](g: A => B): Yoneda[F, B]  // Just compose!
  def lower: F[A]                       // Run once

// Many maps → ONE traversal
Yoneda.lift(bigStructure)
  .map(f1).map(f2).map(f3).map(f4)
  .lower  // Only traverses ONCE with f4∘f3∘f2∘f1
```

### Kan Extensions: Universal Constructions

**Right Kan Extension**: `Ran[G, H, A] = ∀B. (A → G[B]) → H[B]`

```scala
// Continuation-passing style is a Ran!
type Cont[R, A] = Ran[[X] =>> X, [X] =>> R, A]
// = ∀B. (A → B) → R
// = (A → R) → R  (when B = R)
```

**Left Kan Extension**: `Lan[G, H, A] = ∃B. G[B] × (B → H[A])`

```scala
// Coyoneda is a left Kan extension!
type Coyoneda[F[_], A] = Lan[[X] =>> X, F, A]
// = ∃B. B × (B → F[A])
// Makes ANY type constructor a Functor
```

### Natural Transformations

A **natural transformation** `F ~> G` is a polymorphic function:

```scala
trait ~>[F[_], G[_]]:
  def apply[A](fa: F[A]): G[A]

// Laws (naturality): For all f: A → B
// nat[B] ∘ F.map(f) = G.map(f) ∘ nat[A]
```

**Key use**: Interpret Free[F, A] into any monad via F ~> M

### Fix Point: Explicit Recursion

The **fixed point** `Fix[F]` captures recursion:

```scala
case class Fix[F[_]](unfix: F[Fix[F]])

// Val ≅ Fix[ValF] - recursive data as fixed point of functor
```

**Benefit**: Generic algorithms work on ANY recursive type:
```scala
def cata[F[_], A](alg: F[A] => A)(fix: Fix[F]): A  // Works for ANY F!
```

---

## 13. The Full Picture

```
                          ┌─────────────────────────────────────┐
                          │           SPECIFICATION             │
                          │   Σ = (Sorts, Constructors)         │
                          └──────────────┬──────────────────────┘
                                         │
                          ┌──────────────▼──────────────────────┐
                          │         TERM ALGEBRA T(Σ)           │
                          │   Val = Fix[ValF]                   │
                          │   Free algebra over signature       │
                          └──────────────┬──────────────────────┘
                                         │
         ┌───────────────────────────────┼───────────────────────────────┐
         │                               │                               │
         ▼                               ▼                               ▼
┌─────────────────┐            ┌─────────────────┐            ┌─────────────────┐
│   RECURSION     │            │   TRANSFORMS    │            │   ANNOTATIONS   │
│   SCHEMES       │            │   Xform[A,B]    │            │   Cofree[F,A]   │
│                 │            │                 │            │                 │
│ cata: fold      │            │ forward/backward│            │ Typed AST       │
│ ana: unfold     │            │ Profunctor ops  │            │ Comonadic       │
│ hylo: refold    │            │ Registry        │            │ extend/extract  │
│ para: + orig    │            └────────┬────────┘            └────────┬────────┘
└────────┬────────┘                     │                               │
         │                              │                               │
         └──────────────────────────────┼───────────────────────────────┘
                                        │
                          ┌─────────────▼─────────────────────┐
                          │         EFFECTS & DSL             │
                          │   Free[TermOp, A]                 │
                          │   Inspectable, interpretable      │
                          └─────────────┬─────────────────────┘
                                        │
         ┌──────────────────────────────┼──────────────────────────────┐
         │                              │                              │
         ▼                              ▼                              ▼
┌─────────────────┐           ┌─────────────────┐           ┌─────────────────┐
│   NAVIGATION    │           │   VERSIONING    │           │   VALIDATION    │
│   Zipper        │           │   Repo/Patch    │           │   Validated     │
│   Comonad       │           │   Group action  │           │   Applicative   │
│   Lens/Prism    │           │   Hash-consed   │           │   Error accum   │
└─────────────────┘           └─────────────────┘           └─────────────────┘
```

---

## 14. Why This Matters

### Correctness Guarantees

1. **Totality**: Every constructor has a rule → no runtime match errors
2. **Termination**: Structural recursion → transforms always finish
3. **Round-trip**: Isomorphisms → no information loss
4. **Composition**: Homomorphisms compose → build complex from simple

### Practical Benefits

1. **Predictability**: Math tells you what the code will do
2. **Refactoring**: Preserve structure → preserve meaning
3. **Testing**: Properties from math → property-based tests
4. **Optimization**: Hash consing → automatic sharing

### The Bootstrap Property

Because T(Σ) is initial, any interpretation is determined by:
- What each constructor means in the target

This is why Phi can define itself:
```
phi.phi defines Σ_phi
phi2scala.phi defines h: T(Σ_phi) → T(Σ_scala)
Applying h to phi.phi generates the Scala implementation
```

The math guarantees this is well-defined and total.
