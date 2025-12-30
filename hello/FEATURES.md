# Phi Hello World - Features

## The Power of Algebra

This demo embodies a simple but profound idea: **languages are algebraic structures**.

```
Σ = (Sorts, Constructors)     -- signature
T(Σ) = free algebra           -- terms (ASTs)
h: T(Σ₁) → T(Σ₂)              -- homomorphism (transform)
```

Every `.phi` file is a **signature**. Every transform is a **homomorphism**.
The entire system composes algebraically.

---

## What We Have

### 7 Specification Files (~750 lines)

| File | Purpose | Algebra |
|------|---------|---------|
| `phi.phi` | Language for defining languages | Σ_meta |
| `meta.phi` | Runtime semantics (Val, Pat, Expr) | Σ_interp |
| `meta2scala.phi` | Interpreter → Scala | h: T(Σ_interp) → T(Σ_scala) |
| `phi2scala.phi` | Spec → Scala compiler | h: T(Σ_meta) → T(Σ_scala) |
| `hello.phi` | Source language | Σ_hello |
| `scala.phi` | Target language | Σ_scala |
| `hello2scala.phi` | Hello → Scala transform | h: T(Σ_hello) → T(Σ_scala) |

### Scala Implementation (~3,700 lines)

| Package | Files | Lines | Role |
|---------|-------|-------|------|
| `phi.core` | Xform, Attributes, Validation, Zipper, Change, HashConsing, Repo, Algebra | ~1,340 | Core infrastructure |
| `phi.meta` | Core, Lexer, Syntax, MetaInterp | ~340 | Runtime engine |
| `phi.meta.gen` | Eval, Match, Show, Interp | ~280 | Generated from meta2scala.phi |
| `phi.phi` | LangSpec, PhiParser, GrammarInterp, TokenRender | ~570 | Spec parser & grammar |
| `phi.user` | GenHello, RunHello, GenMeta, GenPhi, GenBase | ~620 | Demo drivers |

---

## Key Features

### 1. **Self-Description**
`phi.phi` describes the syntax for `.phi` files — including itself.
This is the bootstrap: the language that defines languages.

### 2. **Bidirectional Grammars**
Grammar rules work both ways:
```
"Hello" IDENT => Hello(SimpleName(IDENT))
```
- **Parse**: `"Hello World"` → `Hello(SimpleName(World))`
- **Render**: `Hello(SimpleName(World))` → `"Hello World"`

### 3. **Compositional Transforms**
Transforms are defined by structural recursion:
```
Hello(name) ↦ PrintLn(StringConcat(StringLit("Hi, "), Name2Scala.forward(name), ...))
```
Each rule is a local equation. Composition is automatic.

### 4. **Algebraic Data Types Everywhere**
Everything is an ADT:
- Source AST: `Greeting = Hello(Name)`
- Target AST: `ScalaStmt = PrintLn(ScalaExpr) | ...`  
- Runtime values: `Val = VCon(String, List[Val]) | VStr | VInt | VList`

### 5. **Pattern Matching as First-Class**
Patterns appear in:
- Grammar rules (parsing)
- Transform rules (LHS)
- Match expressions (interpretation)

### 6. **Two Execution Modes**

| Mode | File | How It Works |
|------|------|--------------|
| **Compiled** | `GenHello.scala` | Rendering hardcoded (what phi2scala generates) |
| **Interpreted** | `RunHello.scala` | Loads specs at runtime, applies transforms |

### 7. **Explicit Xform[A,B] Trait**
Bidirectional transformations are now first-class:
```scala
trait Xform[A, B]:
  def forward(term: Term[A]): Term[B]
  def backward(term: Term[B]): Term[A]
  def roundTrip(term: Term[A]): Boolean
```
- Compose: `xform1.andThen(xform2)`
- Invert: `xform.inverse`
- Registry: `XformRegistry.fromSpec(spec, "Greeting2Scala")`

### 8. **Attribute Grammar**
Explicit inherited/synthesized attribute flows:
```scala
enum FlowDirection:
  case Down   // Inherited (parent → child)
  case Up     // Synthesized (child → parent)
  case Both   // Bidirectional
```
- `InheritedFlow[A]` - e.g., indent level
- `SynthesizedFlow[A]` - e.g., computed type
- `AttrEvaluator.evaluate(term, grammar, getChildren)`

### 9. **Validation & Round-Trip Checks**
```scala
Validation.syntaxRoundTrip(syntax, input)    // parse → render → reparse
Validation.xformRoundTrip(xform, term)       // forward → backward
Validation.grammarRoundTrip(spec, grammar, input)
Validation.grammarCompleteness(spec, grammar) // all constructors covered
```

### 10. **Structured Editing (Zipper + Change)**
Navigate and edit terms structurally:
```scala
val zipper = TermZipper(term)
zipper.down(extract, rebuild).fill(value).up.toTerm
```
Changes are invertible for undo:
```scala
Change.insert(value)  // into hole
Change.replace(term)  // any term
Change.delete         // create hole
change.invert(original) // for undo
```

### 11. **Content-Addressed Storage**
Unison-style hash consing:
```scala
val hash = Hash.compute(term)           // SHA-256 prefix
val (h, canonical) = store.intern(term) // deduplicated
store.get(hash)                         // lookup by hash
```

### 12. **Pijul-Style Repository**
Version control for terms:
```scala
val repo = Repo[Val]()
repo.store(term, Set(Name("hello.greeting")))
val patch = Patch.create("add greeting", change, original)
repo.applyPatch(patch, term)
repo.revertPatch(patch, term)  // undo
repo.createBranch("experiment")
repo.checkout("main")
```

### 13. **Algebraic Structures (Free from Math)**

The algebraic foundation unlocks powerful abstractions:

| Structure | What You Get | Example |
|-----------|--------------|--------|
| **Functor** (map) | Transform contents | `term.map(f)` |
| **Monad** (flatMap) | `for` comprehensions | `for { a <- parse; b <- xform(a) } yield b` |
| **Comonad** (Zipper) | Context-aware transforms | `zipper.extend(z => computeType(z))` |
| **Recursion Schemes** | Generic folds/unfolds | `cata`, `ana`, `hylo`, `para` |
| **Optics** | Composable access | `lens1 andThen lens2 andThen traversal` |
| **Profunctor** (Xform) | Adapt with any function | `xform.dimap(pre)(post)` |
| **Validated** | Parallel error collection | `(v1, v2, v3).mapN(f)` |

```scala
// Recursion scheme: count nodes
val count = cata[Int] {
  case ValF.ConF(_, args) => 1 + args.sum
  case ValF.StrF(_)       => 1
  case _                  => 1
}

// Optics: modify all strings deep in a term
val doubled = Optics.everywhere.modify {
  case VStr(s) => VStr(s + s)
  case v => v
}(term)

// Validated: collect ALL errors, not just first
val result = Validated.map3(check1(x), check2(y), check3(z))(combine)
```

---

## The Algebra in Action

### Signature (from hello.phi)
```
sort Greeting
sort Name
constructor Hello : Name → Greeting
constructor SimpleName : String → Name
```

This defines: `Greeting = Hello(Name)`, `Name = SimpleName(String)`

### Free Algebra (Terms)
```
Hello(SimpleName("World"))
Hello(SimpleName("Phi"))
```
These are elements of T(Σ_hello) — the free algebra over the signature.

### Homomorphism (from hello2scala.phi)
```
rule Greeting2Scala.hello {
  Hello(name) ↦ PrintLn(StringConcat(StringLit("Hi, "), 
                        StringConcat(Name2Scala.forward(name), StringLit("!"))))
}
```

This defines h: T(Σ_hello) → T(Σ_scala) that preserves structure:
- h(Hello(x)) = PrintLn(... h'(x) ...)
- h'(SimpleName(s)) = StringLit(s)

### Composition
```
parse ∘ transform ∘ render : String → String
```
The pipeline composes because each stage is a homomorphism.

---

## Why This Matters

### Correctness by Construction
- Transforms are total (every constructor has a rule)
- Types are preserved (source sort → target sort)
- No ad-hoc string manipulation

### Bidirectionality
- Same grammar parses AND renders
- Same transform can run forward AND backward
- Round-trip: `render(parse(s)) = s`

### Bootstrapping
- `phi.phi` is written in Phi syntax
- `meta2scala.phi` generates `gen/Eval.scala`, `gen/Match.scala`, `gen/Show.scala`
- Generated code matches hand-crafted — **bootstrapping loop is closed**
- `phi2scala.phi` compiles Phi to Scala
- Eventually: `phi2scala(phi.phi)` → self-hosting compiler

### Portability
- `hello.phi` is target-independent
- Only `hello2scala.phi` mentions Scala
- Add `hello2python.phi` for Python output

---

## File Organization

```
hello/
├── examples/           # .phi specifications
│   ├── phi.phi         # meta-language (defines .phi syntax)
│   ├── meta.phi        # interpreter primitives (Val, Pat, Expr, Eval, Match, Show)
│   ├── meta2scala.phi  # GenEval, GenMatch, GenShow → gen/*.scala
│   ├── phi2scala.phi   # spec → Scala compiler  
│   ├── hello.phi       # source language
│   ├── scala.phi       # target language (Scala AST)
│   └── hello2scala.phi # source → target transform
│
└── src/main/scala/phi/
    │   ├── core/           # ← explicit infrastructure
    │   ├── Algebra.scala     # Monad, Comonad, RecursionSchemes, Optics, Validated
    │   ├── Xform.scala       # Xform[A,B] trait, XformRegistry, Profunctor
    │   ├── Attributes.scala  # AttrGrammar, FlowDirection, Attributed
    │   ├── Validation.scala  # round-trip checks, completeness
    │   ├── Zipper.scala      # TermZipper, ZipperContext
    │   ├── Change.scala      # Change[A] enum, ChangeApplicator
    │   ├── HashConsing.scala # Hash, TermHasher, HashConsedStore
    │   └── Repo.scala        # Patch, Name, Branch, Repo[A]
    │
    ├── meta/           # ← runtime + generated
    │   ├── Core.scala      # Val, Env, Pat, Expr, Result ADTs
    │   ├── Lexer.scala     # Lex tokens, TokenStream
    │   ├── Syntax.scala    # Term[A], Syntax[A], Iso
    │   ├── MetaInterp.scala # LangInterpreter (applies rules)
    │   └── gen/            # ← generated from meta2scala.phi
    │       ├── Eval.scala  # expression evaluation
    │       ├── Match.scala # pattern matching
    │       ├── Show.scala  # pretty printing
    │       └── Interp.scala # pattern/substitution helpers
    │
    ├── phi/            # ← spec parser infrastructure
    │   ├── LangSpec.scala    # AST types for language specs
    │   ├── PhiParser.scala   # parser for .phi files
    │   ├── GrammarInterp.scala # bidirectional grammar interpreter
    │   └── TokenRender.scala # RenderCtx, built-in token semantics
    │
    └── user/           # ← demo drivers
        ├── GenBase.scala    # shared generation utilities
        ├── GenHello.scala   # compiled hello demo
        ├── GenMeta.scala    # runs meta2scala code generation
        ├── GenPhi.scala     # runs phi2scala code generation
        └── RunHello.scala   # interpreted version
```

---

## Summary

| Concept | Implementation |
|---------|----------------|
| **Signature** | `sort` + `constructor` declarations |
| **Term** | `Val.VCon(name, args)` |
| **Term with Holes** | `Term[A] = Done(a) \| Hole(label)` |
| **Homomorphism** | `xform` + `rule` declarations |
| **Xform Trait** | `Xform[A,B].forward/backward` |
| **Free algebra** | AST built by constructors |
| **Initial algebra** | No junk, no confusion |
| **Catamorphism** | Pattern-matching transform rules |
| **Attribute Grammar** | `InheritedFlow` + `SynthesizedFlow` |
| **Content Addressing** | `Hash.compute(term)` |
| **Version Control** | `Repo[A]` with `Patch[A]` |
| **Structured Editing** | `TermZipper` + `Change[A]` |
| **Validation** | `Validation.roundTrip` checks |
| **Functor/Monad** | `Term[A].map/flatMap` |
| **Comonad** | `TermZipper.extend/extract` |
| **Recursion Schemes** | `cata/ana/hylo/para` on `Val` |
| **Optics** | `Lens/Prism/Traversal` composition |
| **Profunctor** | `Xform.dimap/lmap/rmap` |
| **Validated** | Parallel error accumulation |

**The entire system is one algebraic structure transforming into another,
with composable abstractions that derive "for free" from the math.**
