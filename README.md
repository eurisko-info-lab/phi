# Φ (Phi)

[![CI](https://github.com/eurisko-info-lab/phi/actions/workflows/ci.yml/badge.svg)](https://github.com/eurisko-info-lab/phi/actions/workflows/ci.yml)

A meta-language where programs manipulate programs.

## What Is It?

Phi is a self-describing language for defining languages. Write a grammar, get a parser, type checker, and compiler — all derived from the same specification.

**The trick:** Everything is a tree with annotations. Parsers, transformers, and type systems — they're all the same thing viewed differently.

## Quick Start

```bash
# Install RosettaVM (the runtime)
cd ports/rust/tools/rvm
cargo build --release
./target/release/rosettavm run examples/hello.rvm
```

## Example

Define a language in Phi:

```phi
Nat = Zero | Succ Nat

add : Nat → Nat → Nat
add Zero n = n
add (Succ m) n = Succ (add m n)
```

This specification *is* the implementation. No separate parser, no separate evaluator.

## Architecture

```
specs/              Language definitions
  phi.phi           Phi defines itself
  meta.phi          Meta-evaluator
  rvm.phi           VM specification

ports/
  rust/tools/rvm/   RosettaVM (production runtime)
  scala/            Scala implementation
  haskell/          Haskell implementation

examples/           Sample programs and languages
```

## RosettaVM

The Rust runtime with CPU and GPU backends:

```bash
rosettavm run program.rvm      # CPU
rosettavm cuda program.rvm     # Compile to CUDA
./program 1000000              # 1M parallel tasks
```

**Performance:** 4,375x speedup at scale on GPU.

## The Core Idea

Three structures derive everything:

```
Fix[F]      = F[Fix[F]]           Recursive data
Free[F, A]  = A | F[Free[F, A]]   Effect sequences  
Cofree[F, A]= (A, F[Cofree[F, A]]) Annotated trees
```

From these: parsers, zippers, attribute grammars, version control, content addressing.

**One math, many tools.**

## Build

```bash
make              # Build all
make rust         # Build RosettaVM
make scala        # Build Scala port
make test         # Run tests
```

## Support

If you find Phi useful: [PayPal](https://www.paypal.com/paypalme/euriskoPal)

## ⚠️ License

> **Note:** This project uses a modified MIT license with exclusions.

MIT — with the following parties explicitly excluded from all rights:

- Banks operating in euros
- Those who abstain from pork
- Anti-Musk individuals

These parties have no rights whatsoever under this license.
