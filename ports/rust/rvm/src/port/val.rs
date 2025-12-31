//! Port Runtime Values
//!
//! The runtime representation of values during evaluation.

use std::fmt;
use std::rc::Rc;
use super::expr::Expr;
use super::env::Env;

/// Runtime values in Port
#[derive(Debug, Clone)]
pub enum Val {
    /// Integer value
    Int(i64),
    
    /// String value
    Str(Rc<String>),
    
    /// Boolean value
    Bool(bool),
    
    /// Unit value
    Unit,
    
    /// Constructor value: Name(args)
    Con(String, Vec<Val>),
    
    /// List value
    List(Vec<Val>),
    
    /// Tuple value
    Tuple(Vec<Val>),
    
    /// Closure: captures environment
    Closure {
        param: String,
        body: Expr,
        env: Env,
    },
    
    /// Builtin function (possibly partially applied)
    Builtin(String, Vec<Val>),
    
    /// Thunk for lazy evaluation
    Thunk(Expr, Env),
}

impl Val {
    // Constructors
    pub fn int(n: i64) -> Self { Val::Int(n) }
    pub fn str(s: impl Into<String>) -> Self { Val::Str(Rc::new(s.into())) }
    pub fn bool(b: bool) -> Self { Val::Bool(b) }
    pub fn unit() -> Self { Val::Unit }
    
    pub fn con(name: impl Into<String>, args: Vec<Val>) -> Self {
        Val::Con(name.into(), args)
    }
    
    pub fn con0(name: impl Into<String>) -> Self {
        Val::Con(name.into(), vec![])
    }
    
    pub fn list(elems: Vec<Val>) -> Self { Val::List(elems) }
    pub fn tuple(elems: Vec<Val>) -> Self { Val::Tuple(elems) }
    
    pub fn closure(param: impl Into<String>, body: Expr, env: Env) -> Self {
        Val::Closure { param: param.into(), body, env }
    }
    
    pub fn builtin(name: impl Into<String>) -> Self {
        Val::Builtin(name.into(), vec![])
    }
    
    // Destructors
    pub fn as_int(&self) -> Option<i64> {
        match self { Val::Int(n) => Some(*n), _ => None }
    }
    
    pub fn as_str(&self) -> Option<&str> {
        match self { Val::Str(s) => Some(s), _ => None }
    }
    
    pub fn as_bool(&self) -> Option<bool> {
        match self { Val::Bool(b) => Some(*b), _ => None }
    }
    
    pub fn as_list(&self) -> Option<&[Val]> {
        match self { Val::List(l) => Some(l), _ => None }
    }
    
    pub fn as_tuple(&self) -> Option<&[Val]> {
        match self { Val::Tuple(t) => Some(t), _ => None }
    }
    
    pub fn as_con(&self) -> Option<(&str, &[Val])> {
        match self { Val::Con(name, args) => Some((name, args)), _ => None }
    }
    
    pub fn is_truthy(&self) -> bool {
        match self {
            Val::Bool(b) => *b,
            Val::Int(n) => *n != 0,
            Val::List(l) => !l.is_empty(),
            Val::Unit => false,
            _ => true,
        }
    }
    
    /// Check structural equality
    pub fn eq(&self, other: &Val) -> bool {
        match (self, other) {
            (Val::Int(a), Val::Int(b)) => a == b,
            (Val::Str(a), Val::Str(b)) => a == b,
            (Val::Bool(a), Val::Bool(b)) => a == b,
            (Val::Unit, Val::Unit) => true,
            (Val::Con(n1, a1), Val::Con(n2, a2)) => {
                n1 == n2 && a1.len() == a2.len() && 
                a1.iter().zip(a2.iter()).all(|(x, y)| x.eq(y))
            }
            (Val::List(a), Val::List(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eq(y))
            }
            (Val::Tuple(a), Val::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(x, y)| x.eq(y))
            }
            _ => false,
        }
    }
}

impl PartialEq for Val {
    fn eq(&self, other: &Self) -> bool {
        Val::eq(self, other)
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Unit => write!(f, "()"),
            // Special case: Const(x) -> just show x (for Prolog)
            Val::Con(name, args) if name == "Const" && args.len() == 1 => {
                write!(f, "{}", args[0])
            }
            Val::Con(name, args) if args.is_empty() => write!(f, "{}", name),
            Val::Con(name, args) => {
                write!(f, "{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Val::List(elems) => {
                write!(f, "[")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, "]")
            }
            Val::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Val::Closure { param, .. } => write!(f, "<closure \\{}>", param),
            Val::Builtin(name, args) if args.is_empty() => write!(f, "<builtin:{}>", name),
            Val::Builtin(name, args) => {
                write!(f, "<builtin:{}(", name)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")>")
            }
            Val::Thunk(_, _) => write!(f, "<thunk>"),
        }
    }
}
