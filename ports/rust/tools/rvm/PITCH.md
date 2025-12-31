# RosettaVM: Code That Never Breaks

## The Problem

Every developer knows this pain:

- **"It works on my machine"** — but not yours
- **Dependency hell** — `lodash@4.17.20` vs `lodash@4.17.21` breaks everything
- **Version conflicts** — library A needs X v2, library B needs X v3
- **Stale caches** — "have you tried clearing your node_modules?"
- **Reproducibility** — good luck building that 2019 project today

The root cause? **We identify code by name and version**, not by what it actually *is*.

## The Solution

**RosettaVM identifies code by its content hash.**

```
add : Int → Int → Int
#a7f3b2c1
```

That hash `#a7f3b2c1` is computed from the function's actual definition. Same code = same hash. Always. Forever.

### No More Version Numbers

Instead of:
```
dependencies:
  lodash: "^4.17.0"  # What does this even mean in 2 years?
```

You get:
```
dependencies:
  map: #8f2a4b7c     # This exact implementation. Forever.
  filter: #3d9e1f0a  # Immutable. Verifiable. Portable.
```

### Names Are Just Aliases

```
store.alias("factorial", #b4c2d1e0)
store.alias("fact", #b4c2d1e0)      # Same hash, different name
store.alias("阶乘", #b4c2d1e0)       # Works in any language!
```

The hash is the truth. Names are convenient labels humans use.

## Why It Matters

| Traditional | RosettaVM |
|-------------|-----------|
| "Did you update to v2.3.1?" | "Use hash #a1b2c3d4" |
| "Which version broke it?" | "Hash #x changed to #y" |
| "Is this the same code?" | Compare hashes. Done. |
| Rebuild from scratch | Cache by hash forever |
| Trust the package manager | Verify the hash yourself |

## The Unison Connection

This isn't theoretical. [Unison](https://unison-lang.org) proved it works:

> "A codebase is a set of (hash, definition) pairs. That's it."

RosettaVM brings this model to a portable, embeddable VM that can run:
- Phi (our meta-language)
- Port (the abstraction layer)
- Any language that compiles to it

## One-Liner

**RosettaVM: A virtual machine where code is addressed by what it is, not what it's called.**

## Try It

```bash
# Hash anything
$ rosettavm hash "hello world"
d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24

# Same input = same hash. Always.
$ rosettavm hash "hello world"
d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24

# Run code by hash
$ rosettavm run myprogram.rvm
42

# Compile to GPU and run 1M parallel tasks
$ rosettavm cuda factorial.rvm
$ nvcc -o factorial factorial.cu && ./factorial 1000000
3628800000000  # 1M factorials in 0.3 seconds
```

## GPU-Accelerated Execution

RosettaVM compiles directly to CUDA for massively parallel GPU execution:

| Task | CPU | GPU | Speedup |
|------|-----|-----|--------|
| 1,000 factorials | 1.4s | 0.31s | **4.5x** |
| 100,000 factorials | ~140s | 0.32s | **437x** |
| 1,000,000 factorials | ~1400s | 0.32s | **4,375x** |

The same RVM bytecode runs on CPU or compiles to CUDA. Content-addressed code meets GPU parallelism.

## The Vision

Imagine a world where:

- **Every function ever written** has a unique, permanent address
- **Sharing code** means sharing a hash (64 characters)
- **Dependencies** are just hashes pointing to other hashes
- **Caching** is trivial (hash exists? use cached result)
- **Refactoring** is safe (same hash = same behavior)
- **GPU acceleration** is automatic for parallel workloads
- **Code search** works across all projects (search by hash)

That world is what RosettaVM enables.

---

*"The best way to predict the future is to invent it." — Alan Kay*

RosettaVM: Invent once, run forever.
