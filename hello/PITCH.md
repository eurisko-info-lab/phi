# Φ-hello: The 172-Line Metaprogramming Core

**One file. Complete algebraic infrastructure. 90% less code.**

---

## The Problem

Building language tools requires:
- Parsers, transformers, analyzers
- Attribute grammars for semantic analysis  
- Zippers for navigation and editing
- Change tracking for undo/redo
- Content addressing for caching

Traditional implementations scatter these across thousands of lines in dozens of files, with redundant patterns everywhere.

## The Insight

**Everything is a tree with annotations.**

- A **Zipper** is a tree annotated with *locations*
- An **Attributed tree** is a tree annotated with *computed values*
- A **Versioned tree** is a tree annotated with *hashes*

One structure: `Cofree[F, A]` — a tree where every node carries an annotation of type `A`.

## The Solution

Φ-hello derives everything from three algebraic structures:

```
μ[F]        = Fix point      (recursive data)
Free[F, A]  = A + F[Free]    (effect sequences)
Cofree[F, A]= A × F[Cofree]  (annotated trees)
```

Plus one universal function:

```scala
Co.ann[I, S](v)(inherit, synthesize, init)
```

This single function implements **all attribute grammars** — inherited attributes flow down, synthesized attributes flow up, in one pass.

## The Numbers

| Metric | Before | After |
|--------|--------|-------|
| Files | 9 | 1 |
| Lines | 1,737 | 172 |
| Concepts | Scattered | Unified |

**90% reduction** with *more* capability, not less.

## What You Get

In 172 lines:

- **Pattern Functor** `V` — one layer of your AST
- **Recursion Schemes** `cata`, `ana`, `hylo` — principled folds
- **Free Monad** `Fr` — inspectable effect sequences  
- **Cofree Comonad** `Co` — universal tree annotation
- **Optics** `Ln`, `Pr`, `Tr` — compositional access
- **Transforms** `X` — bidirectional mappings
- **Edits** `Ed` — algebraic change tracking
- **Validation** `Vd` — parallel error collection
- **Hashing** `H`, `Sto` — content addressing
- **Yoneda** `Yo` — automatic map fusion

All with clean exports via `Φ`.

## The Vision

**Φ** is a meta-language where programs manipulate programs.

This core proves the thesis: *algebraic structure eliminates accidental complexity*.

What took libraries thousands of lines emerges naturally from the mathematics.

---

*"The purpose of abstraction is not to be vague, but to create a new semantic level in which one can be absolutely precise."* — Dijkstra
