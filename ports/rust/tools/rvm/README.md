# RosettaVM

A content-addressed virtual machine implemented in Rust.

## Overview

RosettaVM is a stack-based VM inspired by [Unison's](https://www.unison-lang.org/) content-addressed code model:

- **Content-addressed**: Every function, term, and type is identified by its BLAKE3 hash
- **Names are aliases**: Names resolve to hashes, eliminating versioning problems
- **Portable**: The same code hash runs identically everywhere
- **Stack-based**: Simple, efficient execution model with closures and ADTs

## Building

```bash
cargo build --release
```

## Usage

```bash
# Run an assembly file
rosettavm run examples/hello.rvm

# Interactive REPL
rosettavm repl

# Hash a file or string
rosettavm hash "hello world"
rosettavm hash myfile.rvm

# Evaluate simple expressions
rosettavm eval 42
rosettavm eval "[1, 2, 3]"

# Run built-in tests
rosettavm test
```

## Assembly Language

RosettaVM uses a simple assembly syntax:

```
fn main() {
    push 42      ; push integer
    push 8
    add          ; arithmetic
    print        ; output
    halt
}

fn factorial(n) {
    load 0       ; load argument
    push 1
    le           ; compare
    jf 2         ; conditional jump
    push 1
    ret
    
    load 0
    dup
    push 1
    sub
    call factorial
    mul
    ret
}
```

### Instructions

| Category | Instructions |
|----------|-------------|
| Stack | `push`, `pop`, `dup`, `swap`, `rot`, `over` |
| Env | `load N`, `store N` |
| Arithmetic | `add`, `sub`, `mul`, `div`, `mod`, `neg` |
| Comparison | `eq`, `ne`, `lt`, `le`, `gt`, `ge` |
| Boolean | `not`, `and`, `or` |
| Control | `jump N`, `jt N`, `jf N`, `call`, `ret`, `halt` |
| Functions | `closure`, `apply`, `applyn N` |
| Data | `tuple N`, `list N`, `con Type tag N`, `field N` |
| Lists | `cons`, `head`, `tail`, `isnil`, `len`, `concat` |
| Strings | `strcat`, `strlen`, `strslice` |
| Debug | `print`, `typeof`, `assert`, `trace` |

## Architecture

```
┌─────────────────────────────────────────────┐
│                  RosettaVM                  │
├─────────────┬─────────────┬─────────────────┤
│    Store    │     VM      │    Compiler     │
│  (codebase) │  (runtime)  │   (expr→code)   │
├─────────────┼─────────────┼─────────────────┤
│ Hash→Code   │ Stack       │ AST             │
│ Name→Hash   │ Frames      │ CodeBlock       │
│ Hash→Type   │ Environment │                 │
└─────────────┴─────────────┴─────────────────┘
```

### Content Addressing

Every piece of code is identified by its hash:

```rust
let code = vec![Push(42), Halt];
let block = CodeBlock::new(code);
let hash = store.add_code(block);  // Returns content hash

// Same code = same hash, always
let hash2 = store.add_code(CodeBlock::new(vec![Push(42), Halt]));
assert_eq!(hash, hash2);
```

Names are just aliases:

```rust
store.alias("answer", hash);
store.resolve("answer") == Some(hash)
```

## Modules

| File | Description |
|------|-------------|
| `hash.rs` | BLAKE3 content hashing |
| `value.rs` | Runtime values (Val, Env) |
| `instr.rs` | Instructions and CodeBlock |
| `store.rs` | Content-addressed codebase |
| `vm.rs` | Stack machine execution |
| `parse.rs` | Assembly parser |
| `compile.rs` | Expression compiler |
| `main.rs` | CLI interface |

## Tests

```bash
cargo test           # Unit tests
cargo run -- test    # Integration tests
```

## Relation to Phi/Port

RosettaVM is the bootstrap runtime for the Phi meta-language system:

- **phi.port**: Phi's self-specification in Port syntax
- **meta.port**: The interpreter/evaluator
- **rosettavm.port**: The VM specification (what this implements)

With RosettaVM, you can execute code written in Port/Phi without needing the Scala/Haskell implementations.

## License

MIT
