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

### 9 Scala Files (~900 lines)

| Package | Files | Role |
|---------|-------|------|
| `phi.meta` | Val, Core, Syntax, MetaInterp | Interpreter engine |
| `phi.phi` | LangSpec, PhiParser, GrammarInterp | Spec parser & grammar execution |
| `phi.user` | GenHello, RunHello | Demo drivers |

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
│   ├── meta.phi        # interpreter primitives
│   ├── meta2scala.phi  # interpreter → Scala
│   ├── phi2scala.phi   # spec → Scala compiler  
│   ├── hello.phi       # source language
│   ├── scala.phi       # target language
│   └── hello2scala.phi # source → target transform
│
└── src/main/scala/phi/
    ├── meta/           # ← generated from meta2scala(meta.phi)
    │   ├── Val.scala
    │   ├── Core.scala
    │   ├── Syntax.scala
    │   └── MetaInterp.scala
    │
    ├── phi/            # ← generated from phi2scala(phi.phi)
    │   ├── LangSpec.scala
    │   ├── PhiParser.scala
    │   └── GrammarInterp.scala
    │
    └── user/           # ← generated from hello2scala(hello.phi)
        ├── GenHello.scala   # compiled version
        └── RunHello.scala   # interpreted version
```

---

## Summary

| Concept | Implementation |
|---------|----------------|
| **Signature** | `sort` + `constructor` declarations |
| **Term** | `Val.VCon(name, args)` |
| **Homomorphism** | `xform` + `rule` declarations |
| **Free algebra** | AST built by constructors |
| **Initial algebra** | No junk, no confusion |
| **Catamorphism** | Pattern-matching transform rules |

**The entire system is one algebraic structure transforming into another.**
