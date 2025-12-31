# HVM4 Examples

Examples demonstrating the [Interaction Calculus](https://github.com/HigherOrderCO/hvm4) 
as specified in `specs/hvm.phi`.

## Core Concepts

HVM4 extends lambda calculus with **superpositions** and **duplications**:

| Construct | Syntax | Meaning |
|-----------|--------|---------|
| Lambda | `λx.body` | Function abstraction |
| Application | `(f x)` | Function application |
| Superposition | `&L{a, b}` | Two values in one location |
| Duplication | `!x &L= v; body` | Clone a value for use as x₀ and x₁ |
| Erasure | `&{}` | Black hole (absorbs anything) |

## The Four Core Rules

1. **APP-LAM**: `(λx.b)(a)` → `[x←a]b` (β-reduction)
2. **DUP-SUP**: `!x &L= &L{a,b}` → `x₀←a, x₁←b` (annihilation)
3. **APP-SUP**: `(&L{f,g})(a)` → `!A &L=a; &L{(f A₀),(g A₁)}` (distribution)
4. **DUP-LAM**: `!f &L= λx.b` → creates superposed lambdas (cloning)

## Examples

| File | Description |
|------|-------------|
| [add.hvm](add.hvm) | Basic arithmetic |
| [superposition.hvm](superposition.hvm) | Computing multiple results at once |
| [duplication.hvm](duplication.hvm) | Optimal sharing of computation |
| [church.hvm](church.hvm) | Church numerals and optimal 2^2 |
| [fibonacci.hvm](fibonacci.hvm) | Recursive fib with implicit memoization |
| [parallel-map.hvm](parallel-map.hvm) | Implicit parallelism via superpositions |

## Running

Once the HVM→RVM compiler is complete:

```bash
# Compile HVM to RVM
phi compile examples/hvm/church.hvm -o church.rvm

# Run on RosettaVM
rvm church.rvm
```

## Why This Matters

HVM achieves **optimal reduction** - every subterm is evaluated at most once,
even when duplicated. This emerges automatically from the interaction rules,
without explicit memoization or caching.

On parallel hardware (GPUs), superpositions map directly to parallel threads,
giving near-linear speedups for suitable programs.
