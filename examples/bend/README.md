# Bend Examples

Bend is a high-level functional language with Python-like syntax that compiles
to HVM (High-order Virtual Machine) for massively parallel execution.

## Files

- [hello.bend](hello.bend) - Hello world and basic functions
- [factorial.bend](factorial.bend) - Recursive factorial with switch
- [tree.bend](tree.bend) - Tree data type with fold
- [parallel.bend](parallel.bend) - Parallel computation with superpositions
- [fibonacci.bend](fibonacci.bend) - Recursive Fibonacci with bend
- [list.bend](list.bend) - List operations (map, filter, fold)

## Syntax Overview

```python
# Function definition
def name(arg1: Type1, arg2) -> RetType:
  body

# Type/ADT definition  
type Tree:
  case Node:
    left: Tree
    right: Tree
  case Leaf:
    value: U24

# Pattern matching
match x:
  case Constructor:
    body
  case _:
    default

# Fold (auto-recursive match)
fold tree:
  case Tree/Node:
    tree.left + tree.right  # recursive fields auto-folded
  case Tree/Leaf:
    tree.value

# Bend (manual recursion)
bend x = initial:
  when condition:
    step  # use fork(x) to recurse
  else:
    base

# Switch (numeric)
switch n:
  case 0:
    zero_case
  case _:
    succ_case  # n-1 as n.pred

# Lambda
lambda x, y: x + y
λx: x

# Superposition (parallel values)
{a b}
```

## Compilation Pipeline

```
Bend → HVM → RVM
```

1. **Bend → HVM**: High-level syntax to interaction calculus
   - `def` → `Lam` nodes
   - `match` → `Mat` nodes  
   - `fold` → recursive `Mat` with self-calls
   - `{a b}` → `Sup` nodes
   
2. **HVM → RVM**: Interaction calculus to RosettaVM bytecode
   - Optimal beta reduction
   - Automatic parallelism from superpositions
