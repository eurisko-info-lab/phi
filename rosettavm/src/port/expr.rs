//! Port Expressions
//!
//! The core expression language for Port programs.

use std::rc::Rc;

/// Expressions in Port
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals
    Int(i64),
    String(Rc<String>),
    Bool(bool),
    Unit,
    
    // Variables and constructors
    Var(String),
    Ctor(String, Vec<Expr>),
    
    // Functions
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    
    // Let bindings
    Let(String, Box<Expr>, Box<Expr>),
    LetRec(String, Box<Expr>, Box<Expr>),
    
    // Pattern matching
    Match(Box<Expr>, Vec<MatchCase>),
    
    // Lists
    List(Vec<Expr>),
    Cons(Box<Expr>, Box<Expr>),
    
    // Tuples
    Tuple(Vec<Expr>),
    Proj(Box<Expr>, usize),
    
    // Builtins
    Builtin(String),
    
    // Binary operators (sugar for App(App(Builtin(op), e1), e2))
    BinOp(String, Box<Expr>, Box<Expr>),
    
    // If-then-else (sugar for Match)
    If(Box<Expr>, Box<Expr>, Box<Expr>),
}

impl Expr {
    // Constructors for convenience
    pub fn int(n: i64) -> Self { Expr::Int(n) }
    pub fn string(s: impl Into<String>) -> Self { Expr::String(Rc::new(s.into())) }
    pub fn bool(b: bool) -> Self { Expr::Bool(b) }
    pub fn unit() -> Self { Expr::Unit }
    pub fn var(name: impl Into<String>) -> Self { Expr::Var(name.into()) }
    
    pub fn ctor(name: impl Into<String>, args: Vec<Expr>) -> Self {
        Expr::Ctor(name.into(), args)
    }
    
    pub fn ctor0(name: impl Into<String>) -> Self {
        Expr::Ctor(name.into(), vec![])
    }
    
    pub fn lam(param: impl Into<String>, body: Expr) -> Self {
        Expr::Lam(param.into(), Box::new(body))
    }
    
    pub fn app(func: Expr, arg: Expr) -> Self {
        Expr::App(Box::new(func), Box::new(arg))
    }
    
    pub fn let_(name: impl Into<String>, value: Expr, body: Expr) -> Self {
        Expr::Let(name.into(), Box::new(value), Box::new(body))
    }
    
    pub fn let_rec(name: impl Into<String>, value: Expr, body: Expr) -> Self {
        Expr::LetRec(name.into(), Box::new(value), Box::new(body))
    }
    
    pub fn match_(scrutinee: Expr, cases: Vec<MatchCase>) -> Self {
        Expr::Match(Box::new(scrutinee), cases)
    }
    
    pub fn list(elems: Vec<Expr>) -> Self { Expr::List(elems) }
    
    pub fn cons(head: Expr, tail: Expr) -> Self {
        Expr::Cons(Box::new(head), Box::new(tail))
    }
    
    pub fn tuple(elems: Vec<Expr>) -> Self { Expr::Tuple(elems) }
    
    pub fn proj(tuple: Expr, index: usize) -> Self {
        Expr::Proj(Box::new(tuple), index)
    }
    
    pub fn builtin(name: impl Into<String>) -> Self {
        Expr::Builtin(name.into())
    }
    
    pub fn binop(op: impl Into<String>, left: Expr, right: Expr) -> Self {
        Expr::BinOp(op.into(), Box::new(left), Box::new(right))
    }
    
    pub fn if_(cond: Expr, then_: Expr, else_: Expr) -> Self {
        Expr::If(Box::new(cond), Box::new(then_), Box::new(else_))
    }
    
    // Multi-arg application helper
    pub fn apps(func: Expr, args: Vec<Expr>) -> Self {
        args.into_iter().fold(func, |f, a| Expr::app(f, a))
    }
    
    // Multi-param lambda helper
    pub fn lams(params: Vec<String>, body: Expr) -> Self {
        params.into_iter().rev().fold(body, |b, p| Expr::lam(p, b))
    }
}

/// A case in a match expression
#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: super::pattern::Pattern,
    pub guard: Option<Expr>,
    pub body: Expr,
}

impl MatchCase {
    pub fn new(pattern: super::pattern::Pattern, body: Expr) -> Self {
        MatchCase { pattern, guard: None, body }
    }
    
    pub fn with_guard(mut self, guard: Expr) -> Self {
        self.guard = Some(guard);
        self
    }
}

/// Pretty print an expression
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(n) => write!(f, "{}", n),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Unit => write!(f, "()"),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Ctor(name, args) if args.is_empty() => write!(f, "{}", name),
            Expr::Ctor(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Expr::Lam(param, body) => write!(f, "\\{} -> {}", param, body),
            Expr::App(func, arg) => write!(f, "({} {})", func, arg),
            Expr::Let(name, value, body) => {
                write!(f, "let {} = {} in {}", name, value, body)
            }
            Expr::LetRec(name, value, body) => {
                write!(f, "let rec {} = {} in {}", name, value, body)
            }
            Expr::Match(scrutinee, cases) => {
                write!(f, "match {} {{ ", scrutinee)?;
                for case in cases {
                    write!(f, "{} => {}; ", case.pattern, case.body)?;
                }
                write!(f, "}}")
            }
            Expr::List(elems) => {
                write!(f, "[")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Expr::Cons(head, tail) => write!(f, "({} :: {})", head, tail),
            Expr::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Expr::Proj(tuple, index) => write!(f, "{}.{}", tuple, index),
            Expr::Builtin(name) => write!(f, "<builtin:{}>", name),
            Expr::BinOp(op, left, right) => write!(f, "({} {} {})", left, op, right),
            Expr::If(cond, then_, else_) => {
                write!(f, "if {} then {} else {}", cond, then_, else_)
            }
        }
    }
}
