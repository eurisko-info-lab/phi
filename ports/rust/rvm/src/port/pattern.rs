//! Port Patterns
//!
//! Patterns for pattern matching in Port.

use std::fmt;

/// Patterns for matching values
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard: matches anything, binds nothing
    Wild,
    
    /// Variable: matches anything, binds to name
    Var(String),
    
    /// Constructor pattern: matches a specific constructor
    Ctor(String, Vec<Pattern>),
    
    /// Literal pattern: matches a specific value
    Lit(Literal),
    
    /// As pattern: name @ pattern
    As(String, Box<Pattern>),
    
    /// List pattern: [p1, p2, ...]
    List(Vec<Pattern>),
    
    /// Cons pattern: head :: tail
    Cons(Box<Pattern>, Box<Pattern>),
    
    /// Tuple pattern: (p1, p2, ...)
    Tuple(Vec<Pattern>),
}

/// Literal values in patterns
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

impl Pattern {
    pub fn wild() -> Self { Pattern::Wild }
    pub fn var(name: impl Into<String>) -> Self { Pattern::Var(name.into()) }
    
    pub fn ctor(name: impl Into<String>, args: Vec<Pattern>) -> Self {
        Pattern::Ctor(name.into(), args)
    }
    
    pub fn ctor0(name: impl Into<String>) -> Self {
        Pattern::Ctor(name.into(), vec![])
    }
    
    pub fn int(n: i64) -> Self { Pattern::Lit(Literal::Int(n)) }
    pub fn string(s: impl Into<String>) -> Self { Pattern::Lit(Literal::String(s.into())) }
    pub fn bool(b: bool) -> Self { Pattern::Lit(Literal::Bool(b)) }
    
    pub fn as_(name: impl Into<String>, pattern: Pattern) -> Self {
        Pattern::As(name.into(), Box::new(pattern))
    }
    
    pub fn list(patterns: Vec<Pattern>) -> Self { Pattern::List(patterns) }
    
    pub fn cons(head: Pattern, tail: Pattern) -> Self {
        Pattern::Cons(Box::new(head), Box::new(tail))
    }
    
    pub fn tuple(patterns: Vec<Pattern>) -> Self { Pattern::Tuple(patterns) }
    
    /// Get all variables bound by this pattern
    pub fn bindings(&self) -> Vec<&str> {
        match self {
            Pattern::Wild => vec![],
            Pattern::Var(name) => vec![name.as_str()],
            Pattern::Ctor(_, args) => args.iter().flat_map(|p| p.bindings()).collect(),
            Pattern::Lit(_) => vec![],
            Pattern::As(name, inner) => {
                let mut binds = vec![name.as_str()];
                binds.extend(inner.bindings());
                binds
            }
            Pattern::List(pats) => pats.iter().flat_map(|p| p.bindings()).collect(),
            Pattern::Cons(head, tail) => {
                let mut binds = head.bindings();
                binds.extend(tail.bindings());
                binds
            }
            Pattern::Tuple(pats) => pats.iter().flat_map(|p| p.bindings()).collect(),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Pattern::Wild => write!(f, "_"),
            Pattern::Var(name) => write!(f, "{}", name),
            Pattern::Ctor(name, args) if args.is_empty() => write!(f, "{}", name),
            Pattern::Ctor(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Pattern::Lit(Literal::Int(n)) => write!(f, "{}", n),
            Pattern::Lit(Literal::String(s)) => write!(f, "\"{}\"", s),
            Pattern::Lit(Literal::Bool(b)) => write!(f, "{}", b),
            Pattern::As(name, inner) => write!(f, "{} @ {}", name, inner),
            Pattern::List(pats) => {
                write!(f, "[")?;
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, "]")
            }
            Pattern::Cons(head, tail) => write!(f, "({} :: {})", head, tail),
            Pattern::Tuple(pats) => {
                write!(f, "(")?;
                for (i, p) in pats.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", p)?;
                }
                write!(f, ")")
            }
        }
    }
}
