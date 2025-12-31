//! Port Evaluator
//!
//! Implements the Eval, Apply, and pattern matching xforms from port.phi.

use super::expr::{Expr, MatchCase};
use super::pattern::{Pattern, Literal};
use super::val::Val;
use super::env::Env;

/// Evaluation result
pub type EvalResult = Result<Val, EvalError>;

/// Evaluation errors
#[derive(Debug, Clone)]
pub enum EvalError {
    UnboundVariable(String),
    TypeError(String),
    PatternMatchFailed,
    DivisionByZero,
    IndexOutOfBounds(usize),
    ArityMismatch { expected: usize, got: usize },
    Custom(String),
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnboundVariable(name) => write!(f, "unbound variable: {}", name),
            EvalError::TypeError(msg) => write!(f, "type error: {}", msg),
            EvalError::PatternMatchFailed => write!(f, "pattern match failed"),
            EvalError::DivisionByZero => write!(f, "division by zero"),
            EvalError::IndexOutOfBounds(i) => write!(f, "index out of bounds: {}", i),
            EvalError::ArityMismatch { expected, got } => {
                write!(f, "arity mismatch: expected {}, got {}", expected, got)
            }
            EvalError::Custom(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for EvalError {}

/// The Port evaluator
pub struct Evaluator {
    /// Maximum recursion depth
    pub max_depth: usize,
    /// Current depth
    depth: usize,
    /// Trace evaluation steps
    pub trace: bool,
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            max_depth: 10000,
            depth: 0,
            trace: false,
        }
    }
    
    /// Evaluate an expression in an environment
    pub fn eval(&mut self, expr: &Expr, env: &Env) -> EvalResult {
        self.depth += 1;
        if self.depth > self.max_depth {
            return Err(EvalError::Custom("maximum recursion depth exceeded".into()));
        }
        
        if self.trace {
            eprintln!("{}eval: {}", "  ".repeat(self.depth.min(20)), expr);
        }
        
        let result = self.eval_inner(expr, env);
        
        if self.trace {
            if let Ok(ref v) = result {
                eprintln!("{}=> {}", "  ".repeat(self.depth.min(20)), v);
            }
        }
        
        self.depth -= 1;
        result
    }
    
    fn eval_inner(&mut self, expr: &Expr, env: &Env) -> EvalResult {
        match expr {
            // Literals
            Expr::Int(n) => Ok(Val::int(*n)),
            Expr::String(s) => Ok(Val::Str(s.clone())),
            Expr::Bool(b) => Ok(Val::bool(*b)),
            Expr::Unit => Ok(Val::unit()),
            
            // Variables
            Expr::Var(name) => {
                env.lookup(name)
                    .ok_or_else(|| EvalError::UnboundVariable(name.clone()))
            }
            
            // Constructors
            Expr::Ctor(name, args) => {
                let vals: Result<Vec<_>, _> = args.iter()
                    .map(|a| self.eval(a, env))
                    .collect();
                Ok(Val::con(name.clone(), vals?))
            }
            
            // Lambda
            Expr::Lam(param, body) => {
                Ok(Val::closure(param.clone(), (**body).clone(), env.clone()))
            }
            
            // Application
            Expr::App(func, arg) => {
                let fval = self.eval(func, env)?;
                let aval = self.eval(arg, env)?;
                self.apply(fval, aval)
            }
            
            // Let binding
            Expr::Let(name, value, body) => {
                let v = self.eval(value, env)?;
                let new_env = env.extend();
                new_env.bind(name.clone(), v);
                self.eval(body, &new_env)
            }
            
            // Let rec
            Expr::LetRec(name, value, body) => {
                let new_env = env.extend();
                // Create a placeholder
                new_env.bind(name.clone(), Val::unit());
                // Evaluate the value (which can reference name)
                let v = self.eval(value, &new_env)?;
                // Update the binding
                new_env.bind(name.clone(), v);
                self.eval(body, &new_env)
            }
            
            // Pattern matching
            Expr::Match(scrutinee, cases) => {
                let sval = self.eval(scrutinee, env)?;
                self.eval_match(&sval, cases, env)
            }
            
            // Lists
            Expr::List(elems) => {
                let vals: Result<Vec<_>, _> = elems.iter()
                    .map(|e| self.eval(e, env))
                    .collect();
                Ok(Val::list(vals?))
            }
            
            Expr::Cons(head, tail) => {
                let h = self.eval(head, env)?;
                let t = self.eval(tail, env)?;
                match t {
                    Val::List(mut elems) => {
                        elems.insert(0, h);
                        Ok(Val::list(elems))
                    }
                    _ => Err(EvalError::TypeError("cons requires list".into())),
                }
            }
            
            // Tuples
            Expr::Tuple(elems) => {
                let vals: Result<Vec<_>, _> = elems.iter()
                    .map(|e| self.eval(e, env))
                    .collect();
                Ok(Val::tuple(vals?))
            }
            
            Expr::Proj(tuple, index) => {
                let t = self.eval(tuple, env)?;
                match t {
                    Val::Tuple(elems) => {
                        elems.get(*index)
                            .cloned()
                            .ok_or(EvalError::IndexOutOfBounds(*index))
                    }
                    _ => Err(EvalError::TypeError("projection requires tuple".into())),
                }
            }
            
            // Builtins
            Expr::Builtin(name) => Ok(Val::builtin(name.clone())),
            
            // Binary operators
            Expr::BinOp(op, left, right) => {
                let l = self.eval(left, env)?;
                let r = self.eval(right, env)?;
                self.eval_binop(op, l, r)
            }
            
            // If-then-else
            Expr::If(cond, then_, else_) => {
                let c = self.eval(cond, env)?;
                if c.is_truthy() {
                    self.eval(then_, env)
                } else {
                    self.eval(else_, env)
                }
            }
        }
    }
    
    /// Apply a function value to an argument
    pub fn apply(&mut self, func: Val, arg: Val) -> EvalResult {
        match func {
            Val::Closure { param, body, env } => {
                let new_env = env.extend();
                new_env.bind(param, arg);
                self.eval(&body, &new_env)
            }
            
            Val::Builtin(name, mut args) => {
                args.push(arg);
                self.apply_builtin(&name, args)
            }
            
            _ => Err(EvalError::TypeError(format!("cannot apply non-function: {}", func))),
        }
    }
    
    /// Apply a builtin function
    fn apply_builtin(&mut self, name: &str, args: Vec<Val>) -> EvalResult {
        match (name, args.as_slice()) {
            // Arithmetic (curried)
            ("add", [Val::Int(a), Val::Int(b)]) => Ok(Val::int(a + b)),
            ("sub", [Val::Int(a), Val::Int(b)]) => Ok(Val::int(a - b)),
            ("mul", [Val::Int(a), Val::Int(b)]) => Ok(Val::int(a * b)),
            ("div", [Val::Int(a), Val::Int(b)]) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Val::int(a / b))
                }
            }
            ("mod", [Val::Int(a), Val::Int(b)]) => {
                if *b == 0 {
                    Err(EvalError::DivisionByZero)
                } else {
                    Ok(Val::int(a % b))
                }
            }
            ("neg", [Val::Int(a)]) => Ok(Val::int(-a)),
            
            // Comparison
            ("eq", [a, b]) => Ok(Val::bool(a.eq(b))),
            ("ne", [a, b]) => Ok(Val::bool(!a.eq(b))),
            ("lt", [Val::Int(a), Val::Int(b)]) => Ok(Val::bool(a < b)),
            ("le", [Val::Int(a), Val::Int(b)]) => Ok(Val::bool(a <= b)),
            ("gt", [Val::Int(a), Val::Int(b)]) => Ok(Val::bool(a > b)),
            ("ge", [Val::Int(a), Val::Int(b)]) => Ok(Val::bool(a >= b)),
            
            // Boolean
            ("not", [Val::Bool(b)]) => Ok(Val::bool(!b)),
            ("and", [Val::Bool(a), Val::Bool(b)]) => Ok(Val::bool(*a && *b)),
            ("or", [Val::Bool(a), Val::Bool(b)]) => Ok(Val::bool(*a || *b)),
            
            // List operations
            ("head", [Val::List(l)]) => {
                l.first().cloned().ok_or(EvalError::Custom("head of empty list".into()))
            }
            ("tail", [Val::List(l)]) => {
                if l.is_empty() {
                    Err(EvalError::Custom("tail of empty list".into()))
                } else {
                    Ok(Val::list(l[1..].to_vec()))
                }
            }
            ("cons", [h, Val::List(t)]) => {
                let mut new_list = vec![h.clone()];
                new_list.extend(t.iter().cloned());
                Ok(Val::list(new_list))
            }
            ("null", [Val::List(l)]) => Ok(Val::bool(l.is_empty())),
            ("length", [Val::List(l)]) => Ok(Val::int(l.len() as i64)),
            ("append", [Val::List(a), Val::List(b)]) => {
                let mut result = a.clone();
                result.extend(b.iter().cloned());
                Ok(Val::list(result))
            }
            ("reverse", [Val::List(l)]) => {
                let mut result = l.clone();
                result.reverse();
                Ok(Val::list(result))
            }
            
            // Higher-order list ops
            ("map", [func, Val::List(l)]) => {
                let results: Result<Vec<_>, _> = l.iter()
                    .map(|x| self.apply(func.clone(), x.clone()))
                    .collect();
                Ok(Val::list(results?))
            }
            ("filter", [func, Val::List(l)]) => {
                let mut results = vec![];
                for x in l {
                    let keep = self.apply(func.clone(), x.clone())?;
                    if keep.is_truthy() {
                        results.push(x.clone());
                    }
                }
                Ok(Val::list(results))
            }
            ("foldl", [func, acc, Val::List(l)]) => {
                let mut result = acc.clone();
                for x in l {
                    let partial = self.apply(func.clone(), result)?;
                    result = self.apply(partial, x.clone())?;
                }
                Ok(result)
            }
            ("foldr", [func, acc, Val::List(l)]) => {
                let mut result = acc.clone();
                for x in l.iter().rev() {
                    let partial = self.apply(func.clone(), x.clone())?;
                    result = self.apply(partial, result)?;
                }
                Ok(result)
            }
            
            // String operations
            ("concat", [Val::Str(a), Val::Str(b)]) => {
                Ok(Val::str(format!("{}{}", a, b)))
            }
            ("show", [v]) => Ok(Val::str(format!("{}", v))),
            ("strlen", [Val::Str(s)]) => Ok(Val::int(s.len() as i64)),
            
            // I/O
            ("print", [v]) => {
                println!("{}", v);
                Ok(Val::unit())
            }
            ("trace", [v]) => {
                eprintln!("[trace] {}", v);
                Ok(v.clone())
            }
            
            // Option
            ("Some", [v]) => Ok(Val::con("Some", vec![v.clone()])),
            
            // List constructors
            ("Cons", [h, t]) => {
                match t {
                    Val::List(l) => {
                        let mut new_list = vec![h.clone()];
                        new_list.extend(l.iter().cloned());
                        Ok(Val::list(new_list))
                    }
                    _ => Ok(Val::con("Cons", vec![h.clone(), t.clone()]))
                }
            }
            
            // Partial application
            _ => Ok(Val::Builtin(name.to_string(), args)),
        }
    }
    
    /// Evaluate binary operator
    fn eval_binop(&mut self, op: &str, left: Val, right: Val) -> EvalResult {
        match op {
            "+" => self.apply_builtin("add", vec![left, right]),
            "-" => self.apply_builtin("sub", vec![left, right]),
            "*" => self.apply_builtin("mul", vec![left, right]),
            "/" => self.apply_builtin("div", vec![left, right]),
            "%" => self.apply_builtin("mod", vec![left, right]),
            "==" => self.apply_builtin("eq", vec![left, right]),
            "!=" => self.apply_builtin("ne", vec![left, right]),
            "<" => self.apply_builtin("lt", vec![left, right]),
            "<=" | "=<" => self.apply_builtin("le", vec![left, right]),
            ">" => self.apply_builtin("gt", vec![left, right]),
            ">=" => self.apply_builtin("ge", vec![left, right]),
            "&&" => self.apply_builtin("and", vec![left, right]),
            "||" => self.apply_builtin("or", vec![left, right]),
            "::" => self.apply_builtin("cons", vec![left, right]),
            "++" => self.apply_builtin("append", vec![left, right]),
            _ => Err(EvalError::Custom(format!("unknown operator: {}", op))),
        }
    }
    
    /// Pattern matching evaluation
    fn eval_match(&mut self, scrutinee: &Val, cases: &[MatchCase], env: &Env) -> EvalResult {
        for case in cases {
            if let Some(bindings) = self.try_match(&case.pattern, scrutinee) {
                let new_env = env.extend();
                for (name, val) in bindings {
                    new_env.bind(name, val);
                }
                
                // Check guard if present
                if let Some(ref guard) = case.guard {
                    let guard_val = self.eval(guard, &new_env)?;
                    if !guard_val.is_truthy() {
                        continue;
                    }
                }
                
                return self.eval(&case.body, &new_env);
            }
        }
        Err(EvalError::PatternMatchFailed)
    }
    
    /// Try to match a pattern against a value, returning bindings if successful
    fn try_match(&self, pattern: &Pattern, value: &Val) -> Option<Vec<(String, Val)>> {
        match (pattern, value) {
            // Wildcard matches anything
            (Pattern::Wild, _) => Some(vec![]),
            
            // Variable binds the value
            (Pattern::Var(name), v) => Some(vec![(name.clone(), v.clone())]),
            
            // Constructor pattern
            (Pattern::Ctor(pname, pargs), Val::Con(vname, vargs)) => {
                if pname != vname || pargs.len() != vargs.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (p, v) in pargs.iter().zip(vargs.iter()) {
                    let sub_bindings = self.try_match(p, v)?;
                    bindings.extend(sub_bindings);
                }
                Some(bindings)
            }
            
            // Literal patterns
            (Pattern::Lit(Literal::Int(pn)), Val::Int(vn)) if pn == vn => Some(vec![]),
            (Pattern::Lit(Literal::String(ps)), Val::Str(vs)) if ps == vs.as_str() => Some(vec![]),
            (Pattern::Lit(Literal::Bool(pb)), Val::Bool(vb)) if pb == vb => Some(vec![]),
            
            // As pattern
            (Pattern::As(name, inner), v) => {
                let mut bindings = vec![(name.clone(), v.clone())];
                bindings.extend(self.try_match(inner, v)?);
                Some(bindings)
            }
            
            // List pattern
            (Pattern::List(pats), Val::List(vals)) => {
                if pats.len() != vals.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (p, v) in pats.iter().zip(vals.iter()) {
                    bindings.extend(self.try_match(p, v)?);
                }
                Some(bindings)
            }
            
            // Cons pattern
            (Pattern::Cons(phead, ptail), Val::List(vals)) => {
                if vals.is_empty() {
                    return None;
                }
                let mut bindings = self.try_match(phead, &vals[0])?;
                bindings.extend(self.try_match(ptail, &Val::list(vals[1..].to_vec()))?);
                Some(bindings)
            }
            
            // Tuple pattern
            (Pattern::Tuple(pats), Val::Tuple(vals)) => {
                if pats.len() != vals.len() {
                    return None;
                }
                let mut bindings = vec![];
                for (p, v) in pats.iter().zip(vals.iter()) {
                    bindings.extend(self.try_match(p, v)?);
                }
                Some(bindings)
            }
            
            // Empty list patterns
            (Pattern::Ctor(name, args), Val::List(vals)) 
                if (name == "Nil" || name == "[]") && args.is_empty() && vals.is_empty() => {
                Some(vec![])
            }
            
            // No match
            _ => None,
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Evaluator::new()
    }
}

/// Convenience function to evaluate an expression
pub fn eval(expr: &Expr, env: &Env) -> EvalResult {
    Evaluator::new().eval(expr, env)
}

/// Evaluate with tracing enabled
pub fn eval_trace(expr: &Expr, env: &Env) -> EvalResult {
    let mut evaluator = Evaluator::new();
    evaluator.trace = true;
    evaluator.eval(expr, env)
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::env::standard_env;
    
    #[test]
    fn test_arithmetic() {
        let env = standard_env();
        let expr = Expr::binop("+", Expr::int(2), Expr::int(3));
        assert_eq!(eval(&expr, &env).unwrap(), Val::int(5));
    }
    
    #[test]
    fn test_lambda() {
        let env = standard_env();
        let expr = Expr::app(
            Expr::lam("x", Expr::binop("+", Expr::var("x"), Expr::int(1))),
            Expr::int(5)
        );
        assert_eq!(eval(&expr, &env).unwrap(), Val::int(6));
    }
    
    #[test]
    fn test_let() {
        let env = standard_env();
        let expr = Expr::let_(
            "x",
            Expr::int(10),
            Expr::binop("*", Expr::var("x"), Expr::int(2))
        );
        assert_eq!(eval(&expr, &env).unwrap(), Val::int(20));
    }
    
    #[test]
    fn test_list() {
        let env = standard_env();
        let expr = Expr::list(vec![Expr::int(1), Expr::int(2), Expr::int(3)]);
        let result = eval(&expr, &env).unwrap();
        assert_eq!(result.as_list().unwrap().len(), 3);
    }
    
    #[test]
    fn test_pattern_match() {
        let env = standard_env();
        let expr = Expr::match_(
            Expr::list(vec![Expr::int(1), Expr::int(2)]),
            vec![
                MatchCase::new(Pattern::list(vec![]), Expr::int(0)),
                MatchCase::new(
                    Pattern::cons(Pattern::var("h"), Pattern::var("t")),
                    Expr::var("h")
                ),
            ]
        );
        assert_eq!(eval(&expr, &env).unwrap(), Val::int(1));
    }
}
