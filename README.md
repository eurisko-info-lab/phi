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

### Prerequisites

**For RVM (Rust)** - the cornerstone:
```bash
# Install Rust (Linux/macOS)
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env
```

**For Scala Phi** (optional):
```bash
# Install SDKMAN, then SBT
curl -s "https://get.sdkman.io" | bash
sdk install java 21-tem    # or any JDK 11+
sdk install sbt
```

**For Haskell Phi** (optional):
```bash
# Install GHCup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc
ghcup install cabal
```

### Install RVM (RosettaVM) - The Cornerstone

```bash
# One command to build and install
make install

# Or step by step:
make rust          # Build RVM
make install-rvm   # Install to ~/bin/rvm
```

Make sure `~/bin` is in your PATH:
```bash
export PATH="$HOME/bin:$PATH"  # Add to ~/.bashrc or ~/.zshrc
```

### Build Everything

```bash
make              # Build all ports (scala, rust, haskell)
make test         # Run all tests
make clean        # Clean all build artifacts
```

### Build by Language

| Language | Prerequisites | Build | Test | Clean |
|----------|--------------|-------|------|-------|
| **Rust** (RVM) | `rustc`, `cargo` | `make rust` | `make test-rust` | `make clean-rust` |
| **Scala** (Phi) | `sbt`, JDK 11+ | `make scala` | `make test-scala` | `make clean-scala` |
| **Haskell** (Phi) | `ghc`, `cabal` | `make haskell` | `make test-haskell` | `make clean-haskell` |

### Run

```bash
make run-rvm      # Run Rust RVM
make run-phi      # Run Scala phi interpreter
```

## Project Structure

```
phi/
├── specs/                       # Core Phi language specifications
│   ├── phi.phi                  # Self-describing Phi spec
│   ├── meta.phi                 # Meta-language definition
│   ├── rvm.phi                  # RosettaVM specification
│   └── xforms/                  # Transformations
│       ├── phi2rvm.phi          # Phi to RVM
│       ├── phi2scala.phi        # Phi to Scala
│       └── meta2scala.phi       # Meta to Scala
├── examples/                    # Example .phi programs
│   ├── languages/               # Language implementations
│   ├── type-theory/             # Type theory examples
│   ├── xforms/                  # Transformation examples
│   └── tests/                   # Test cases
├── docs/                        # Documentation
├── ports/                       # Language implementations
│   ├── rust/
│   │   ├── tools/rvm/           # RosettaVM (Cargo)
│   │   └── specs/               # Rust port specs
│   ├── scala/
│   │   ├── tools/phi/           # Phi interpreter (SBT)
│   │   └── specs/scala.phi      # Scala port spec
│   └── haskell/
│       ├── tools/phi/           # Phi interpreter (Cabal)
│       └── specs/haskell.phi    # Haskell port spec
├── Makefile                     # Build orchestration
├── README.md                    # This file
├── FEATURES.md, MATH.md, PITCH.md, PROMPT.md
└── ...
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
| `cat.phi` | Category theory (functors, monads) |
| `coc.phi` | Calculus of Constructions |
| `hkt.phi` | Higher-kinded types and type classes |
| `poly.phi` | Polymorphic lambda calculus |
| `stlc-nat.phi` | Simply-typed lambda calculus |
| `calculus.phi` | Lambda calculus variants |
| `cubical.phi` | Cubical type theory |
| `minimal.phi` | Minimal dependently-typed language |

See `specs/` for core definitions:
- `phi.phi` - Self-describing Phi specification
- `meta.phi` - Meta-language for transformations
- `rvm.phi` - RosettaVM target specification

## License

MIT

## References

- [Recursion Schemes](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)
- [Data Types à la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf)
- [Cofree Comonads](https://blog.sigfpe.com/2014/05/cofree-meets-free.html)
- [Bidirectional Programming](https://www.cis.upenn.edu/~bcpierce/papers/lenses-full.pdf)
