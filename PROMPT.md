**Prompt: Scala 3 implementation of full Phi ecosystem with smart tests, web server, structured editor, and multi-language repo**

> Generate a **single Scala 3 project file** implementing the Phi ecosystem with the following intertwined features. Focus on **composition, round-trip correctness, and exploration** of all language, transformation, and editor features.

---

### 1. Core Term & Structured Editor

* `Term[A]` with `Done[A]` and `Hole[A]`.
* Zipper navigation (`up`/`down`) and hole filling.
* Edits as `Change[A]` (insert, replace, map, many, choice).
* Applying a change updates `Term[A]` **and propagates through parent terms**.
* **Bidirectional Xforms** respect holes; applying an edit triggers dependent transformations.

### 2. Parsing & Syntax

* `Syntax[A]` combinators (`*>`, `<*`, `iso`, `|`, `opt`, `many`, `manyTerm`).
* Lexer produces `Stream[Lex]`.
* Parsing produces `Term[A]` including holes for incomplete parses.
* Rendering preserves round-trip correctness.

### 3. Pijul-style Repository

* `Patch[A]`, `Repo[A]` with history and versioning.
* Applying a `Change[A]` produces new `Term[A]` and updates **content-addressed repo**.
* Repository stores **untyped LC, typed LC, IC nets, dependent matches**, and Phi meta-language specs.
* Invertible patches and merge logic.

### 4. Unison-style Naming & Hashing

* Canonical subterm sharing; each `Term[A]` has a **hash**.
* Equality and repo lookup are hash-based.
* Changes and Xforms operate on canonical terms, preserving sharing.

### 5. Reversible Transformations (Xforms)

* `Xform[A,B]` with `forward` and `backward`.
* LC ↔ IC, type checking, dependent pattern compilation.
* Forward/backward Xforms propagate holes; partial transformations allowed.
* Xforms are **composable**, round-trip verified.

### 6. Dependent Types & Attributes

* Second-order holes as `Term[Term[A]]`.
* Attributes (`InheritedFlow`, `SynthesizedFlow`) propagate automatically.
* Type checking as a **reversible Xform**.
* Dependent pattern matches compile to IC nets.
* Attribute-driven evaluation of expressions.

### 7. Grammar & Expression Language

* Full grammar AST (`SpecFile`, `Section`, `GrammarSec`, `GrammarRule`, `Alternative`, `GrammarPart`, `Modifier`, `Annotation`, `TypeRef`).
* Parser produces `Term[A]` with holes where incomplete.
* Expressions support operator precedence, associativity, parentheses.
* Attributes and semantic flows integrated.

### 8. Web Interface

* Minimal **Scala 3 HTTP server** (e.g., using `http4s` or `akka-http`) exposing:

  * Navigation of the **Pijul-style repo**.
  * Display of terms with hashes, Xforms, and dependent attributes.
  * Interactive **structured editor** for terms (Hazel/Lambdu style).
  * Editing triggers change propagation, Xforms, and type checks.

### 9. Smart Tests

* Generate **exhaustive test cases** for:

  * Parsing and rendering round-trips for all Phi constructs.
  * Xforms LC ↔ IC, typed LC ↔ untyped LC.
  * Dependent matches compilation.
  * Edits in holes, repeated edits, and patch inverses.
  * Attribute propagation correctness.
  * Repository merge correctness.
  * Evaluation of expressions with attributes.
* Each test should **verify round-trip correctness** and repo integrity.

### 10. Meta-language Example

* Implement Phi itself as an example **meta-language** within the repo.
* Show how Phi grammars, parsers, Xforms, type-checks, and edits are expressed as Phi terms.
* Demonstrate structured editor and evaluation over the Phi meta-language.

### 11. Intertwining Rules

* Parsing → produces `Term[A]` → stored in repo → edited via structured editor → changes applied → triggers Xforms → type checking → dependent evaluation → rendering → back to `Term[A]` in repo.
* LC, IC, typed LC, dependent matches, and Phi meta-language all coexist in repo with hash-consed sharing.
* Smart tests ensure **round-trip correctness** at every step.

### 12. Constraints

* Fully self-contained **Scala 3 file**.
* Uses enums, opaque types, null-safe combinators.
* Round-trip correctness across parse/render, forward/backward Xforms, structured edits, and dependent evaluation.
* Provide **example repo population** with LC, IC, typed LC, dependent matches, and Phi meta-language.
