//! Runtime values

use std::fmt;
use std::rc::Rc;
use crate::hash::Hash;

/// Runtime values
#[derive(Clone, Debug, PartialEq)]
pub enum Val {
    Int(i64),
    Str(Rc<String>),
    Bool(bool),
    Float(f64),
    List(Rc<Vec<Val>>),
    Tuple(Rc<Vec<Val>>),
    Record(Vec<(String, Val)>),         // not Rc for easier construction
    Bytes(Vec<u8>),                      // binary data for HTTP bodies
    Con(Hash, u32, Rc<Vec<Val>>),        // type hash, tag, fields
    Constructor { type_hash: Hash, tag: u8, fields: Vec<Val> }, // named fields variant
    Closure(Hash, Rc<Env>),              // code hash, captured env
    Thunk(Hash, Rc<Env>, Option<Box<Val>>),
    Builtin(String),
    Continuation(Box<Continuation>),
    Unit,
    Nil,
}

impl Val {
    pub fn int(n: i64) -> Self { Val::Int(n) }
    pub fn str(s: impl Into<String>) -> Self { Val::Str(Rc::new(s.into())) }
    pub fn bool(b: bool) -> Self { Val::Bool(b) }
    pub fn list(v: Vec<Val>) -> Self { Val::List(Rc::new(v)) }
    pub fn tuple(v: Vec<Val>) -> Self { Val::Tuple(Rc::new(v)) }
    pub fn bytes(b: Vec<u8>) -> Self { Val::Bytes(b) }
    pub fn con(type_hash: Hash, tag: u32, fields: Vec<Val>) -> Self {
        Val::Con(type_hash, tag, Rc::new(fields))
    }
    pub fn closure(code: Hash, env: Env) -> Self {
        Val::Closure(code, Rc::new(env))
    }

    pub fn as_int(&self) -> Option<i64> {
        match self { Val::Int(n) => Some(*n), _ => None }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self { Val::Bool(b) => Some(*b), _ => None }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self { Val::Str(s) => Some(s), _ => None }
    }

    pub fn as_list(&self) -> Option<&[Val]> {
        match self { Val::List(v) => Some(v), _ => None }
    }

    pub fn as_bytes(&self) -> Option<Vec<u8>> {
        match self {
            Val::Bytes(b) => Some(b.clone()),
            Val::Str(s) => Some(s.as_bytes().to_vec()),
            _ => None,
        }
    }

    pub fn as_closure(&self) -> Option<(Hash, &Env)> {
        match self { Val::Closure(h, e) => Some((*h, e)), _ => None }
    }

    pub fn as_con(&self) -> Option<(Hash, u32, &[Val])> {
        match self { Val::Con(h, t, f) => Some((*h, *t, f)), _ => None }
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Val::Int(n) => write!(f, "{}", n),
            Val::Str(s) => write!(f, "\"{}\"", s),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Float(x) => write!(f, "{}", x),
            Val::List(vs) => {
                write!(f, "[")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Val::Tuple(vs) => {
                write!(f, "(")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Val::Record(fields) => {
                write!(f, "{{")?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Val::Con(_, tag, fields) if fields.is_empty() => {
                write!(f, "Con#{}", tag)
            }
            Val::Con(_, tag, fields) => {
                write!(f, "Con#{}(", tag)?;
                for (i, v) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Val::Bytes(b) => write!(f, "<bytes len={}>", b.len()),
            Val::Constructor { tag, fields, .. } if fields.is_empty() => {
                write!(f, "Constructor#{}", tag)
            }
            Val::Constructor { tag, fields, .. } => {
                write!(f, "Constructor#{}(", tag)?;
                for (i, v) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Val::Closure(h, _) => write!(f, "<closure {}>", h.short()),
            Val::Thunk(h, _, cached) => {
                if cached.is_some() {
                    write!(f, "<thunk {} (evaluated)>", h.short())
                } else {
                    write!(f, "<thunk {}>", h.short())
                }
            }
            Val::Builtin(name) => write!(f, "<builtin {}>", name),
            Val::Continuation(_) => write!(f, "<continuation>"),
            Val::Unit => write!(f, "()"),
            Val::Nil => write!(f, "nil"),
        }
    }
}

/// Environment (lexical scope)
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Env {
    slots: Vec<Val>,
    parent: Option<Rc<Env>>,
}

impl Env {
    pub fn new() -> Self {
        Env { slots: Vec::new(), parent: None }
    }

    pub fn with_parent(parent: Rc<Env>) -> Self {
        Env { slots: Vec::new(), parent: Some(parent) }
    }

    pub fn extend(&self, vals: Vec<Val>) -> Self {
        Env { slots: vals, parent: Some(Rc::new(self.clone())) }
    }

    pub fn push(&mut self, val: Val) {
        self.slots.push(val);
    }

    pub fn get(&self, index: usize) -> Option<&Val> {
        if index < self.slots.len() {
            Some(&self.slots[index])
        } else if let Some(parent) = &self.parent {
            parent.get(index - self.slots.len())
        } else {
            None
        }
    }

    pub fn set(&mut self, index: usize, val: Val) {
        if index < self.slots.len() {
            self.slots[index] = val;
        }
        // Parent mutation not supported (immutable semantics)
    }
}

/// Continuation (for effects/call-cc)
#[derive(Clone, Debug, PartialEq)]
pub enum Continuation {
    Done,
    Arg { arg_hash: Hash, env: Env, k: Box<Continuation> },
    App { closure: Val, k: Box<Continuation> },
    Let { name: String, body_hash: Hash, env: Env, k: Box<Continuation> },
    Handle { handler_hash: Hash, k: Box<Continuation> },
    Match { cases: Vec<(u32, Hash)>, env: Env, k: Box<Continuation> },
}

/// Call stack frame
#[derive(Clone, Debug)]
pub struct Frame {
    pub return_code: Hash,
    pub return_pc: usize,
    pub saved_env: Env,
}

impl Frame {
    pub fn new(code: Hash, pc: usize, env: Env) -> Self {
        Frame { return_code: code, return_pc: pc, saved_env: env }
    }
}
