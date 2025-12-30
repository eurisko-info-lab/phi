# Φ-Hello: Algebraic Metaprogramming in Scala 3

```
╔═══════════════════════════════════════════════════════════════════════════╗
║  Everything is a tree with annotations.                                    ║
║  From 4 type classes and 4 structures, all follows.                        ║
╚═══════════════════════════════════════════════════════════════════════════╝
```

## Overview

Φ-Hello is a minimal but complete demonstration of algebraic metaprogramming techniques in Scala 3. In ~1500 lines of densely documented code, it provides:

- **Core.scala** (~500 lines): Complete algebraic foundation
- **Meta.scala** (~300 lines): Pattern matching and evaluation
- **Syntax.scala** (~400 lines): Bidirectional parsing
- **Lang.scala** (~400 lines): .phi specification language
- **Run.scala** (~300 lines): Examples and runner

## Quick Start

```bash
cd hello
sbt run   # Run the demo
```

## Architecture

### The Four Type Classes

```scala
trait Functor[F[_]]    // Transform contents preserving structure
trait Monad[M[_]]      // Sequence dependent computations
trait Comonad[W[_]]    // Context-dependent computations  
trait ~>[F, G]         // Natural transformation between functors
```

### The Four Structures

```scala
enum V[+A]                              // Pattern functor (one layer)
case class Fix[F[_]](unfix: F[Fix[F]]) // Fixed point (recursion)
enum Free[F[_], A]                      // Free monad (effects as data)
case class Cofree[F[_], A]              // Cofree comonad (annotated trees)
```

### The Key Insight

**Everything is a tree with annotations.**

This single observation unifies:
- **Zippers** = trees + locations
- **Attributes** = trees + computed values
- **Versions** = trees + content hashes

All three are instances of `Cofree[V, A]` where `A` is the annotation type.

## Core Features

### Recursion Schemes

```scala
// Catamorphism: generic fold (bottom-up)
val nodeCount = cata[Int] {
  case C(_, args) => 1 + args.sum
  case _ => 1
}(ast)

// Anamorphism: generic unfold (top-down)
val peano = ana[Int] { n =>
  if n <= 0 then C("Zero", Nil) else C("Succ", List(n - 1))
}(5)

// Hylomorphism: fused unfold-then-fold
val fib = hylo(algebra, coalgebra)(n)
```

### Navigation with Zippers

```scala
val modified = Zipper.from(ast)
  .go(List(0, 1))        // Navigate to path
  .map(_.update(f))      // Modify at focus
  .map(Zipper.toVal)     // Convert back
```

### Parallel Validation

```scala
val result = (validateName zip validateAge zip validateEmail)
  .map { case ((n, a), e) => Person(n, a, e) }
// Collects ALL errors, not just the first one!
```

### Bidirectional Parsing

```scala
val syntax: Syntax[Expr] = 
  (intLit ~ symbol("+") ~ intLit)
    .imap({ case ((a, _), b) => Add(a, b) }, 
          { case Add(a, b) => ((a, ()), b) })

syntax.parse("1 + 2")   // Some(Add(1, 2))
syntax.render(Add(1,2)) // "1 + 2"
```

## .phi Specifications

Define complete languages in `.phi` files:

```
// Define types
sort Expr

// Define constructors
Expr = Lit(value: Int)
     | Add(left: Expr, right: Expr)
     | Mul(left: Expr, right: Expr)

// Define syntax
grammar Expr {
  Lit <- /[0-9]+/
  Add <- Expr "+" Expr
  Mul <- Expr "*" Expr
}

// Define transformations
xform simplify : Expr -> Expr {
  Add(Lit(0), e) => e
  Mul(Lit(1), e) => e
}
```

## Examples

The `examples/` directory contains several .phi specifications:

| File | Description |
|------|-------------|
| `hello.phi` | Minimal "Hello World" language |
| `core.phi` | Meta-circular Core definition with HKT |
| `phi.phi` | Self-describing Phi specification |
| `cat.phi` | Category theory (functors, monads) |
| `hkt.phi` | Higher-kinded types and type classes |
| `poly.phi` | Polymorphic lambda calculus |
| `stlc-nat.phi` | Simply-typed lambda calculus |
| `calculus.phi` | Lambda calculus variants |
| `cubical.phi` | Cubical type theory |
| `minimal.phi` | Minimal dependently-typed language |

## Project Structure

```
hello/
├── build.sbt                    # SBT configuration
├── README.md                    # This file
├── src/main/scala/phi/
│   ├── Core.scala              # Algebraic foundation
│   ├── Meta.scala              # Evaluation and patterns
│   ├── Syntax.scala            # Bidirectional parsing
│   ├── Lang.scala              # .phi specification parser
│   └── Run.scala               # Examples and entry point
└── examples/
    ├── hello.phi               # Basic example
    ├── core.phi                # Meta-circular definition
    └── ...                     # More examples
```

## Philosophy

1. **Minimal Primitives**: Everything derives from a small set of concepts
2. **Maximum Density**: Every line teaches something
3. **Practical Theory**: Abstract math with concrete applications
4. **Self-Describing**: The system can define itself

## License

MIT

## References

- [Recursion Schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)
- [Data Types à la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)
- [Cofree Comonads](https://blog.sigfpe.com/2014/05/cofree-meets-free.html)
- [Bidirectional Programming](https://www.cis.upenn.edu/~bcpierce/papers/lenses-full.pdf)
