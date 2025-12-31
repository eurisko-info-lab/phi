# Phi: One Spec, All Tools

## The Problem

Building a language requires:
- Parser (hundreds of lines)
- Type checker (hundreds more)
- Evaluator (hundreds more)
- Compiler (hundreds more)

Four separate implementations of the same conceptual thing.

## The Insight

They're all tree transformations.

```
Source  →  AST     (parsing)
AST     →  Types   (checking)  
AST     →  Values  (evaluating)
AST     →  Target  (compiling)
```

What if one specification generated all of them?

## The Solution

Phi is a language where the grammar *is* the implementation.

```phi
Expr = Num Int
     | Add Expr Expr
     | Mul Expr Expr

eval : Expr → Int
eval (Num n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
```

This is complete. Parser derived from constructors. Evaluator from equations.

## Why It Works

Everything is `Cofree[F, A]` — a tree where every node carries an annotation.

- **Parser:** Annotate with source positions
- **Type checker:** Annotate with types
- **Evaluator:** Annotate with values
- **Compiler:** Annotate with target code

Same structure. Different annotations. All derived.

## The Result

| Traditional | Phi |
|-------------|-----|
| 4 implementations | 1 specification |
| Bugs in sync | Single source of truth |
| Change 4 places | Change 1 place |
| Weeks of work | Hours |

## GPU-Accelerated

RosettaVM compiles Phi to CUDA:

```bash
rosettavm cuda program.rvm
./program 1000000  # 1M parallel executions
```

**4,375x speedup** at scale. Same spec, CPU or GPU.

## One-Liner

**A language where the grammar is the implementation, running on CPU or GPU.**
