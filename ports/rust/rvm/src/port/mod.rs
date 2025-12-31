//! Port: The Universal Implementation Abstraction
//!
//! Port is the intermediate representation between Phi specifications and
//! concrete language implementations. This is the Rust port of port.phi.
//!
//! # Architecture
//!
//! ```text
//! phi.phi   → High-level specs (sorts, constructors, xforms)
//! port.phi  → Implementation abstraction (types, functions, patterns)
//! rust/port → This module: executable Port in Rust
//! ```
//!
//! # Key Types
//!
//! - [`Val`] - Runtime values (integers, strings, constructors, closures)
//! - [`Expr`] - Expressions (literals, lambdas, application, match)
//! - [`Pattern`] - Patterns for matching (wildcard, var, constructor)
//! - [`Type`] - Types (primitives, functions, lists, refs)
//! - [`Env`] - Evaluation environment (name → value bindings)

pub mod types;
pub mod expr;
pub mod pattern;
pub mod val;
pub mod env;
pub mod eval;
pub mod parser;
pub mod phi_loader;

pub use types::*;
pub use expr::*;
pub use pattern::*;
pub use val::*;
pub use env::*;
pub use eval::*;
pub use parser::*;
pub use phi_loader::*;
