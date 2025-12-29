# Phi Hello World - Bootstrapping Demo

This is a minimal standalone demonstration of the **Phi bootstrapping concept**:
a language that can define languages, including itself.

## Overview

The Hello World demo shows the complete pipeline:

```
hello.phi          →  Parse "Hello World"  →  Hello(SimpleName("World"))
                              ↓
hello2scala.phi    →  Transform AST        →  ScalaObject(MainDef(PrintLn(...)))
                              ↓
scala.phi          →  Render Scala code    →  object Hello { def main... }
```

## Language Specs

### 1. `phi.phi` - The Meta-Language
Defines the syntax for writing `.phi` files themselves. This is the foundation
for bootstrapping - the language that describes languages.

**Key constructs:**
- `language Name { ... }` - Define a language spec
- `sort Name` - Declare a type/sort
- `constructor Name : Type → Type` - Declare a constructor
- `grammar name { pattern => Result }` - Define parsing rules
- `xform Name : Source ⇄ Target` - Declare a bidirectional transform
- `rule Name.case { pattern ↦ result }` - Define transform rules

### 2. `hello.phi` - Source Language
A tiny language with just one construct: `Hello <name>`

```
language Hello {
  sort Greeting
  sort Name
  constructor Hello : Name → Greeting
  constructor SimpleName : String → Name
  grammar greeting { "Hello" IDENT => Hello(SimpleName(IDENT)) }
}
```

### 3. `scala.phi` - Target Language
Defines the Scala AST subset we generate:

```
language Scala {
  sort ScalaExpr; sort ScalaStmt; sort ScalaDecl; sort ScalaFile
  constructor ScalaObject : String → ScalaDecl → ScalaFile
  constructor MainDef : ScalaStmt → ScalaDecl
  constructor PrintLn : ScalaExpr → ScalaStmt
  constructor StringLit : String → ScalaExpr
  constructor StringConcat : ScalaExpr → ScalaExpr → ScalaExpr
}
```

### 4. `hello2scala.phi` - Transform Spec
Maps Hello AST to Scala AST:

```
rule Greeting2Scala.hello {
  Hello(name) ↦ ScalaObject(StringLit("Hello"), 
                  MainDef(PrintLn(StringConcat(StringLit("Hi, "), 
                          StringConcat(Name2Scala.forward(name), 
                                       StringLit("!"))))))
}
```

## Running the Demo

```bash
cd hello
sbt "runMain phi.GenHello"
```

**Output:**
```
[4] Parsing: "Hello World"
    ✓ AST: Hello(SimpleName(String(World)))

[5] Applying Greeting2Scala transform...
    ✓ Scala AST: ScalaObject(StringLit(Hello), MainDef(PrintLn(...)))

[6] Rendering Scala code...
    ✓ Generated:
object Hello:
  def main(args: Array[String]): Unit =
    println("Hi, " + "World" + "!")
```

Generated file: `tmp/Hello.scala`

## Project Structure

```
hello/
├── build.sbt                    # SBT build (Scala 3.7.4)
├── project/build.properties     # sbt.version=1.11.7
├── examples/
│   ├── phi.phi                  # Meta-language spec
│   ├── hello.phi                # Source language
│   ├── scala.phi                # Target language  
│   └── hello2scala.phi          # Transform spec
├── src/main/scala/phi/
│   ├── Val.scala                # AST values: VCon(name, args)
│   ├── LangSpec.scala           # Spec data types
│   ├── PhiParser.scala          # Parse .phi files
│   ├── Syntax.scala             # Bidirectional parsing
│   ├── GrammarInterp.scala      # Grammar interpreter
│   ├── MetaInterp.scala         # Transform interpreter
│   └── GenHello.scala           # Main driver
└── tmp/
    └── Hello.scala              # Generated output
```

## The Bootstrapping Concept

The key insight is that `phi.phi` uses the same constructs it defines:
- It declares `sort`, `constructor`, `grammar`, `xform`, `rule`
- Using the syntax: `sort Sort`, `constructor SortDecl : String → Decl`, etc.

This self-description is what makes Phi a **meta-language** - it can define
any language, including itself. The Hello World demo is the simplest possible
instance of this pattern.

## Minimal Implementation (~500 lines)

| File              | Lines | Purpose                          |
|-------------------|-------|----------------------------------|
| Val.scala         | ~15   | AST representation               |
| LangSpec.scala    | ~60   | Spec data types                  |
| PhiParser.scala   | ~120  | Parse .phi files                 |
| Syntax.scala      | ~120  | Bidirectional parsing primitives |
| GrammarInterp.scala| ~80  | Execute grammar rules            |
| MetaInterp.scala  | ~100  | Execute transform rules          |
| GenHello.scala    | ~100  | Demo driver                      |

## Next Steps

To extend this demo:
1. Add more Hello constructs (e.g., `Goodbye`, `Question`)
2. Add more Scala constructs (e.g., `Val`, `If`, `Match`)
3. Make the renderer use the `scala.phi` grammar bidirectionally
4. Bootstrap: use phi.phi to parse phi.phi itself!
