# RosettaVM: Code That Never Breaks

## The Problem

Code breaks because we identify it by name and version.

- `lodash@4.17.20` vs `lodash@4.17.21` — which one broke your build?
- "It works on my machine" — but which version of which dependency?
- Stale caches, version conflicts, reproducibility nightmares

## The Solution

**Identify code by what it is, not what it's called.**

```
factorial : Int → Int
#b4c2d1e0
```

That hash is computed from the function's actual implementation. Same code = same hash. Forever.

## Why It Matters

| Before | After |
|--------|-------|
| "Update to v2.3.1" | "Use #a1b2c3d4" |
| "Which version broke it?" | "Hash changed from #x to #y" |
| "Is this the same code?" | Compare hashes |
| Rebuild everything | Cache by hash forever |
| Trust package manager | Verify hash yourself |

## GPU-Accelerated

RosettaVM compiles to CUDA for massively parallel execution:

```bash
rosettavm cuda factorial.rvm
nvcc -o factorial factorial.cu
./factorial 1000000
```

**Results:**

| Scale | CPU | GPU | Speedup |
|-------|-----|-----|---------|
| 1,000 tasks | 1.4s | 0.31s | 4.5x |
| 100,000 tasks | ~140s | 0.32s | 437x |
| 1,000,000 tasks | ~23min | 0.32s | **4,375x** |

Same bytecode. CPU or GPU. Your choice.

## One-Liner

**A VM where code is addressed by what it is, and runs on CPU or GPU.**
