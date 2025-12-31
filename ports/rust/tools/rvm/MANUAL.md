# RosettaVM User Manual

## Table of Contents

1. [Installation](#installation)
2. [Quick Start](#quick-start)
3. [Command Reference](#command-reference)
4. [Universal Interpreter (Phi Languages)](#universal-interpreter-phi-languages)
5. [Assembly Language](#assembly-language)
6. [Data Types](#data-types)
7. [Control Flow](#control-flow)
8. [Functions](#functions)
9. [The Codebase](#the-codebase)
10. [REPL Guide](#repl-guide)
11. [Examples](#examples)
12. [Troubleshooting](#troubleshooting)

---

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/yourname/phi.git
cd phi/rosettavm

# Build release version
cargo build --release

# Optional: install to PATH
cargo install --path .
```

### Verify Installation

```bash
rosettavm --help
rosettavm test
```

---

## Quick Start

### Hello World

Create `hello.rvm`:

```asm
fn main() {
    push "Hello, World!"
    print
    halt
}
```

Run it:

```bash
rosettavm run hello.rvm
```

Output:
```
"Hello, World!"
()
```

### Simple Arithmetic

Create `calc.rvm`:

```asm
fn main() {
    push 6
    push 7
    mul          ; 6 * 7
    halt
}
```

```bash
rosettavm run calc.rvm
```

Output: `42`

---

## Command Reference

### `rosettavm run <file.rvm>`

Execute an assembly file.

```bash
rosettavm run program.rvm
```

The program must define a `main` function. Execution begins there and continues until `halt` or `return` from main.

### `rosettavm repl`

Start an interactive session.

```bash
rosettavm repl
```

REPL commands:
- `:quit` or `:q` — Exit
- `:help` or `:h` — Show help
- `:names` — List defined names and their hashes
- `:clear` — Reset the store

### `rosettavm hash <input>`

Compute the BLAKE3 hash of a string or file.

```bash
# Hash a string
rosettavm hash "hello"

# Hash a file
rosettavm hash myfile.rvm
```

Hashes are 64 hex characters (256 bits).

### `rosettavm eval <expr>`

Evaluate a simple expression.

```bash
rosettavm eval 42
rosettavm eval "[1, 2, 3]"
```

### `rosettavm test`

Run the built-in test suite to verify the installation.

```bash
rosettavm test
```

### `rosettavm cuda <file.rvm>`

Compile RVM bytecode to CUDA C code for GPU execution.

```bash
rosettavm cuda program.rvm
# Generates program.cu

# Compile and run with nvcc
nvcc -o program program.cu
./program           # Single task
./program 1000000   # 1 million parallel tasks
```

The generated CUDA code runs each task on a separate GPU thread. Results are summed and printed. Supports recursive functions with per-call-frame environments.

**Performance:**

| Task | CPU | GPU | Speedup |
|------|-----|-----|---------|
| 1,000 factorials | 1.4s | 0.31s | **4.5x** |
| 100,000 factorials | ~140s | 0.32s | **437x** |
| 1,000,000 factorials | ~1400s | 0.32s | **4,375x** |

### `rosettavm par <benchmark> [args]`

Run parallel CPU benchmarks using Rayon.

```bash
rosettavm par fib 35      # Parallel fibonacci
rosettavm par map 1000000 # Parallel map
```

### `rosettavm bench`

Run CPU vs GPU benchmarks for comparison.

```bash
rosettavm bench
```

---

## Universal Interpreter (Phi Languages)

### `rosettavm phi <spec.phi> <source> [-- <query>]`

The **universal interpreter** mode allows RosettaVM to execute programs written in any language defined by a Phi specification.

**Components:**
- `spec.phi` — A Phi language specification (grammar + transformations)
- `source` — A program written in that language
- `query` — Optional: a query/goal to evaluate

**Examples:**

```bash
# Run a λProlog program
rosettavm phi λProlog.phi quicksort.pl -- "qsort([5,4,8,2,4,1], X)."

# Just load and show the program
rosettavm phi λProlog.phi quicksort.pl

# Run a STLC type checker
rosettavm phi stlc.phi terms.stlc -- "typecheck(lam x. x)"

# Run a calculator
rosettavm phi calc.phi expressions.calc
```

**How it works:**
1. Load the Phi spec to understand the target language
2. Parse the source file using the language's grammar
3. If a query is provided, run it via the language's entry xform (e.g., `Solve`)
4. Otherwise, evaluate the program's main expression

**Supported Language Features:**
- Sorts, constructors, and grammars from Phi specs
- Automatic detection of entry points (`Solve`, `Eval`, `run`, etc.)
- Prolog-style clause parsing for logic languages

**Example: Quicksort in λProlog**

`quicksort.pl`:
```prolog
qsort([], []).
qsort([H|T], Sorted) :-
    partition(H, T, Less, Greater),
    qsort(Less, SortedLess),
    qsort(Greater, SortedGreater),
    append(SortedLess, [H|SortedGreater], Sorted).
```

```bash
$ rosettavm phi λProlog.phi quicksort.pl -- "qsort([3,1,4,1,5], X)."
📦 Loaded language: λProlog
   5 sorts, 11 constructors, 7 xforms
   Entry xform: Solve
📄 Parsing source (911 bytes)...
   Parsed 7 clauses
❓ Query: qsort([3,1,4,1,5], X).

🔍 Solving: qsort([3,1,4,1,5], X).

✓ Solution found:
  X = [1, 1, 3, 4, 5]
```

Run the built-in test suite.

```bash
rosettavm test
```

---

## Assembly Language

### Syntax

```asm
; This is a comment
# This is also a comment

fn function_name(arg1, arg2) {
    instruction operand
    instruction
    ...
}
```

### Literals

| Type | Syntax | Example |
|------|--------|---------|
| Integer | Decimal | `push 42`, `push -17` |
| Float | Decimal with `.` | `push 3.14` |
| String | Double-quoted | `push "hello"` |
| Boolean | `true`/`false` | `push true` |
| Unit | `()` or `unit` | `push ()` |
| Nil | `nil` | `push nil` |
| Hash | `#` + 64 hex | `push #a1b2...` |

### Escape Sequences in Strings

| Escape | Meaning |
|--------|---------|
| `\n` | Newline |
| `\t` | Tab |
| `\r` | Carriage return |
| `\\` | Backslash |
| `\"` | Double quote |

---

## Data Types

### Primitive Types

| Type | Description | Examples |
|------|-------------|----------|
| `Int` | 64-bit signed integer | `0`, `-42`, `9999` |
| `Float` | 64-bit floating point | `3.14`, `-0.5` |
| `Bool` | Boolean | `true`, `false` |
| `String` | UTF-8 string | `"hello"` |
| `Unit` | Empty value | `()` |
| `Nil` | Null/empty list | `nil` |

### Composite Types

#### Lists

```asm
push 1
push 2
push 3
mklist 3      ; creates [1, 2, 3]
```

List operations:
- `head` — First element
- `tail` — All but first
- `cons` — Prepend element
- `len` — Length
- `isnil` — Check if empty
- `concat` — Join two lists
- `index` — Get element at index

#### Tuples

```asm
push 1
push "hello"
push true
tuple 3       ; creates (1, "hello", true)
```

Access with `field N`:
```asm
field 0       ; get first element
field 1       ; get second element
```

#### Constructors (ADTs)

```asm
; Create: Some(42)
push 42
con Option 0 1    ; type=Option, tag=0 (Some), 1 field

; Create: None
con Option 1 0    ; type=Option, tag=1 (None), 0 fields
```

Pattern matching:
```asm
testtag 0         ; is it tag 0?
jf else_branch
unpack 1          ; extract 1 field
; ... handle Some case
```

---

## Control Flow

### Jumps

| Instruction | Description |
|-------------|-------------|
| `jump N` | Jump N instructions forward (negative = backward) |
| `jt N` / `jumpif N` | Jump if top of stack is `true` |
| `jf N` / `jumpifnot N` | Jump if top of stack is `false` |

### Example: If-Then-Else

```asm
fn abs(x) {
    load 0          ; load x
    push 0
    lt              ; x < 0?
    jf positive     ; if not, skip negation
    load 0
    neg             ; negate
    ret
positive:
    load 0
    ret
}
```

Note: Labels like `positive:` are not yet supported in the parser. Use relative offsets:

```asm
fn abs(x) {
    load 0
    push 0
    lt
    jf 3            ; skip 3 instructions
    load 0
    neg
    ret
    load 0
    ret
}
```

### Example: Loop

```asm
fn sum_to_n(n) {
    push 0          ; accumulator
    load 0          ; load n
loop:
    dup
    push 0
    eq
    jt done         ; if n == 0, done
    rot             ; acc n -> n acc
    over            ; n acc -> n acc n
    add             ; n (acc + n)
    swap            ; (acc + n) n
    push 1
    sub             ; (acc + n) (n - 1)
    jump -10        ; back to loop
done:
    pop             ; remove n
    ret             ; return accumulator
}
```

---

## Functions

### Defining Functions

```asm
fn name(param1, param2, ...) {
    ; function body
    ret             ; or halt for main
}
```

### Calling Functions

```asm
push arg1
push arg2
call function_name
```

Arguments are passed on the stack. Access with `load N`:
- `load 0` — First argument
- `load 1` — Second argument
- etc.

### Tail Calls

Use `tailcall` instead of `call` + `ret` for tail-recursive functions:

```asm
fn factorial_tail(n, acc) {
    load 0          ; n
    push 1
    le
    jf recurse
    load 1          ; return acc
    ret
recurse:
    load 0          ; n
    push 1
    sub             ; n - 1
    load 0          ; n
    load 1          ; acc
    mul             ; n * acc
    tailcall factorial_tail
}
```

### Closures

```asm
fn make_adder(x) {
    closure add_x 1   ; capture 1 value from environment
    ret
}

fn add_x(y) {
    load 0            ; captured x
    load 1            ; argument y
    add
    ret
}
```

Apply closures with `apply`:

```asm
push 10
call make_adder     ; returns closure
push 5
apply               ; calls closure with 5, returns 15
```

---

## The Codebase

### Content Addressing

Every piece of code is identified by its BLAKE3 hash:

```bash
$ rosettavm hash "push 42"
# Returns: 64-character hex hash
```

### Name Resolution

Names are aliases for hashes. In the REPL:

```
λ> :names
  main -> a1b2c3d4
  factorial -> e5f6g7h8
  Unit -> 11111111
  Bool -> 22222222
```

### Hash Properties

1. **Deterministic**: Same code → same hash
2. **Collision-resistant**: Different code → different hash
3. **Portable**: Hash is the same on any machine
4. **Cacheable**: Results can be cached by hash

---

## REPL Guide

### Starting the REPL

```bash
rosettavm repl
```

You'll see:
```
RosettaVM REPL (type :help for commands, :quit to exit)
λ>
```

### Evaluating Expressions

```
λ> 42
42

λ> [1, 2, 3]
[1, 2, 3]

λ> "hello"
"hello"
```

### Commands

| Command | Description |
|---------|-------------|
| `:quit` / `:q` | Exit the REPL |
| `:help` / `:h` | Show available commands |
| `:names` | List all defined names with hashes |
| `:clear` | Reset the codebase |

### Limitations

The current REPL only evaluates simple literals. Full expression evaluation requires loading assembly files.

---

## Examples

### Fibonacci

```asm
fn fib(n) {
    load 0
    push 2
    lt
    jf recurse
    load 0
    ret
recurse:
    load 0
    push 1
    sub
    call fib
    load 0
    push 2
    sub
    call fib
    add
    ret
}

fn main() {
    push 10
    call fib
    halt
}
```

### List Sum

```asm
fn sum(list) {
    load 0
    isnil
    jf recurse
    push 0
    ret
recurse:
    load 0
    head
    load 0
    tail
    call sum
    add
    ret
}

fn main() {
    push 1
    push 2
    push 3
    push 4
    push 5
    mklist 5
    call sum
    halt
}
```

### String Manipulation

```asm
fn greet(name) {
    push "Hello, "
    load 0
    strcat
    push "!"
    strcat
    ret
}

fn main() {
    push "World"
    call greet
    print
    halt
}
```

---

## Troubleshooting

### Common Errors

#### Stack Underflow
```
Error: stack underflow
```
**Cause**: Trying to pop from empty stack.
**Fix**: Ensure each operation has enough values on the stack.

#### Undefined Hash
```
Error: undefined hash: a1b2c3d4
```
**Cause**: Calling a function that doesn't exist.
**Fix**: Check function names for typos.

#### Type Mismatch
```
Error: type mismatch: expected int, got string
```
**Cause**: Operation received wrong type.
**Fix**: Check stack contents before operations.

#### Parse Error
```
Parse error at 5:10: expected '}'
```
**Cause**: Syntax error in assembly file.
**Fix**: Check line 5, column 10 for missing braces.

### Debugging Tips

1. **Use `trace`**: Prints value without consuming it
   ```asm
   push 42
   trace       ; prints "[trace] 42"
   ; 42 still on stack
   ```

2. **Use `print`**: Print top of stack
   ```asm
   push "debug point 1"
   print
   ```

3. **Run with debug flag** (if available):
   ```bash
   RUST_LOG=debug rosettavm run program.rvm
   ```

4. **Check types with `typeof`**:
   ```asm
   push 42
   typeof
   print       ; prints "Int"
   ```

---

## Appendix: Instruction Reference

### Stack Operations

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `push V` | `→ V` | Push value |
| `pop` | `A →` | Discard top |
| `dup` | `A → A A` | Duplicate top |
| `swap` | `A B → B A` | Swap top two |
| `rot` | `A B C → B C A` | Rotate top three |
| `over` | `A B → A B A` | Copy second to top |

### Arithmetic

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `add` | `A B → (A+B)` | Addition |
| `sub` | `A B → (A-B)` | Subtraction |
| `mul` | `A B → (A*B)` | Multiplication |
| `div` | `A B → (A/B)` | Division |
| `mod` | `A B → (A%B)` | Modulo |
| `neg` | `A → (-A)` | Negate |

### Comparison

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `eq` | `A B → (A==B)` | Equal |
| `ne` | `A B → (A≠B)` | Not equal |
| `lt` | `A B → (A<B)` | Less than |
| `le` | `A B → (A≤B)` | Less or equal |
| `gt` | `A B → (A>B)` | Greater than |
| `ge` | `A B → (A≥B)` | Greater or equal |

### Boolean

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `not` | `A → (¬A)` | Logical NOT |
| `and` | `A B → (A∧B)` | Logical AND |
| `or` | `A B → (A∨B)` | Logical OR |

### Control Flow

| Instruction | Description |
|-------------|-------------|
| `jump N` | Relative jump |
| `jt N` / `jumpif N` | Jump if true |
| `jf N` / `jumpifnot N` | Jump if false |
| `call F` | Call function |
| `tailcall F` | Tail call |
| `ret` / `return` | Return from function |
| `halt` | Stop execution |

### Data

| Instruction | Description |
|-------------|-------------|
| `tuple N` | Create N-tuple |
| `mklist N` | Create N-list |
| `con T tag N` | Create constructor |
| `field N` | Get tuple field |
| `unpack N` | Extract constructor fields |
| `testtag N` | Test constructor tag |

### Lists

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `cons` | `H T → [H\|T]` | Prepend |
| `head` | `[H\|T] → H` | First element |
| `tail` | `[H\|T] → T` | Rest of list |
| `isnil` | `L → Bool` | Is empty? |
| `len` | `L → Int` | Length |
| `concat` | `A B → A++B` | Concatenate |
| `index` | `L I → L[I]` | Get element |

### Strings

| Instruction | Stack Effect | Description |
|-------------|--------------|-------------|
| `strcat` | `A B → A++B` | Concatenate |
| `strlen` | `S → Int` | Length |
| `strslice` | `S I J → S[I:J]` | Substring |

### Debug

| Instruction | Description |
|-------------|-------------|
| `print` | Print top value |
| `typeof` | Push type name |
| `trace` | Print without consuming |
| `assert` | Check condition |
| `nop` | No operation |

---

*RosettaVM v0.1.0 — Content-addressed code execution*
