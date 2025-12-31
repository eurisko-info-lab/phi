# RosettaVM Manual

## Installation

```bash
cargo build --release
```

## Commands

### run

Execute RVM assembly on CPU.

```bash
rosettavm run program.rvm
```

### cuda

Compile RVM to CUDA for GPU execution.

```bash
rosettavm cuda program.rvm
nvcc -o program program.cu
./program          # single task
./program 1000000  # 1M parallel tasks
```

Output is the sum of all task results.

### par

Run parallel CPU benchmarks.

```bash
rosettavm par fib 35
```

### repl

Interactive mode.

```bash
rosettavm repl
```

Commands: `:quit`, `:help`, `:names`, `:clear`

### hash

Compute BLAKE3 hash.

```bash
rosettavm hash "hello"
rosettavm hash file.rvm
```

### test

Run test suite.

```bash
rosettavm test
```

## Assembly Language

### Basic Structure

```asm
fn name(arg1, arg2) {
    ; instructions
    ret
}

fn main() {
    ; entry point
    halt
}
```

### Instructions

**Stack:** `push`, `pop`, `dup`, `swap`, `rot`, `over`

**Environment:** `load N`, `store N`

**Arithmetic:** `add`, `sub`, `mul`, `div`, `mod`, `neg`

**Comparison:** `eq`, `ne`, `lt`, `le`, `gt`, `ge`

**Boolean:** `not`, `and`, `or`

**Control:** `jump N`, `jumpif N`, `jumpifnot N`, `call fn`, `ret`, `halt`

**Data:** `cons`, `head`, `tail`, `isnil`

### Example: Factorial

```asm
fn fact(n) {
    load 0          ; n
    push 2
    lt              ; n < 2?
    jumpif 8        ; jump to base case
    
    load 0          ; n
    load 0          ; n
    push 1
    sub             ; n-1
    call fact       ; fact(n-1)
    mul             ; n * fact(n-1)
    ret

    push 1          ; base case
    ret
}

fn main() {
    push 10
    call fact
    halt
}
```

## Content Addressing

Every function has a unique hash:

```rust
let hash = store.add_code(block);  // Returns BLAKE3 hash
store.alias("factorial", hash);    // Name is just an alias
```

Same code = same hash. Always.

## GPU Execution

The CUDA backend:

1. Translates RVM bytecode to CUDA C
2. Each GPU thread runs one independent task
3. Supports recursion with per-call-frame environments
4. Results collected and summed

**Performance:**

- ~300ms overhead (kernel launch, memory transfer)
- After overhead: 1 task ≈ 1M tasks (all parallel)
- 4,375x speedup at 1M scale

## Limitations

- GPU recursion limited by thread stack size (64KB default)
- No GPU support for closures yet
- String operations CPU-only
