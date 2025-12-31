//! Port Environment
//!
//! Variable bindings for evaluation.

use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use super::val::Val;

/// Environment: maps names to values
#[derive(Debug, Clone)]
pub struct Env {
    bindings: Rc<RefCell<HashMap<String, Val>>>,
    parent: Option<Box<Env>>,
}

impl Env {
    /// Create an empty environment
    pub fn new() -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: None,
        }
    }
    
    /// Create a child environment
    pub fn extend(&self) -> Self {
        Env {
            bindings: Rc::new(RefCell::new(HashMap::new())),
            parent: Some(Box::new(self.clone())),
        }
    }
    
    /// Bind a name to a value
    pub fn bind(&self, name: impl Into<String>, val: Val) {
        self.bindings.borrow_mut().insert(name.into(), val);
    }
    
    /// Look up a name
    pub fn lookup(&self, name: &str) -> Option<Val> {
        if let Some(val) = self.bindings.borrow().get(name) {
            return Some(val.clone());
        }
        if let Some(parent) = &self.parent {
            return parent.lookup(name);
        }
        None
    }
    
    /// Check if a name is bound
    pub fn contains(&self, name: &str) -> bool {
        if self.bindings.borrow().contains_key(name) {
            return true;
        }
        if let Some(parent) = &self.parent {
            return parent.contains(name);
        }
        false
    }
    
    /// List all bindings (for debugging)
    pub fn all_bindings(&self) -> Vec<(String, Val)> {
        let mut result = vec![];
        self.collect_bindings(&mut result);
        result
    }
    
    fn collect_bindings(&self, result: &mut Vec<(String, Val)>) {
        for (k, v) in self.bindings.borrow().iter() {
            result.push((k.clone(), v.clone()));
        }
        if let Some(parent) = &self.parent {
            parent.collect_bindings(result);
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Env::new()
    }
}

/// Create an environment with standard builtins
pub fn standard_env() -> Env {
    let env = Env::new();
    
    // Arithmetic
    env.bind("add", Val::builtin("add"));
    env.bind("sub", Val::builtin("sub"));
    env.bind("mul", Val::builtin("mul"));
    env.bind("div", Val::builtin("div"));
    env.bind("mod", Val::builtin("mod"));
    env.bind("neg", Val::builtin("neg"));
    
    // Comparison
    env.bind("eq", Val::builtin("eq"));
    env.bind("ne", Val::builtin("ne"));
    env.bind("lt", Val::builtin("lt"));
    env.bind("le", Val::builtin("le"));
    env.bind("gt", Val::builtin("gt"));
    env.bind("ge", Val::builtin("ge"));
    
    // Boolean
    env.bind("not", Val::builtin("not"));
    env.bind("and", Val::builtin("and"));
    env.bind("or", Val::builtin("or"));
    
    // List operations
    env.bind("head", Val::builtin("head"));
    env.bind("tail", Val::builtin("tail"));
    env.bind("cons", Val::builtin("cons"));
    env.bind("null", Val::builtin("null"));
    env.bind("length", Val::builtin("length"));
    env.bind("append", Val::builtin("append"));
    env.bind("reverse", Val::builtin("reverse"));
    env.bind("map", Val::builtin("map"));
    env.bind("filter", Val::builtin("filter"));
    env.bind("foldl", Val::builtin("foldl"));
    env.bind("foldr", Val::builtin("foldr"));
    
    // String operations
    env.bind("concat", Val::builtin("concat"));
    env.bind("show", Val::builtin("show"));
    env.bind("strlen", Val::builtin("strlen"));
    
    // I/O
    env.bind("print", Val::builtin("print"));
    env.bind("trace", Val::builtin("trace"));
    
    // Boolean values
    env.bind("True", Val::con0("True"));
    env.bind("False", Val::con0("False"));
    env.bind("true", Val::bool(true));
    env.bind("false", Val::bool(false));
    
    // Option
    env.bind("None", Val::con0("None"));
    env.bind("Some", Val::builtin("Some"));
    
    // List constructors
    env.bind("Nil", Val::list(vec![]));
    env.bind("Cons", Val::builtin("Cons"));
    
    env
}
