//! RosettaVM Library - WebAssembly bindings and FFI
//!
//! This module provides the API for running Phi specs in the browser
//! and network FFI for autonomous agents.

pub mod hash;
pub mod value;
pub mod instr;
pub mod store;
pub mod vm;
pub mod parse;
pub mod compile;
pub mod port;
pub mod phi_compiler;

// Network FFI (native only, with network feature)
#[cfg(all(not(target_arch = "wasm32"), feature = "network"))]
pub mod ffi;

// Re-export parallel only for native (uses rayon)
#[cfg(not(target_arch = "wasm32"))]
pub mod parallel;

// Re-export parallel for WASM too but we need a stub
#[cfg(target_arch = "wasm32")]
pub mod parallel {
    // Stub module for WASM - no parallel execution
    pub fn parallel_enabled() -> bool { false }
}

// Re-export CUDA modules only on native
#[cfg(not(target_arch = "wasm32"))]
pub mod cuda;
#[cfg(not(target_arch = "wasm32"))]
pub mod cuda_codegen;

// Re-exports for library usage
pub use hash::Hash;
pub use value::Val;
pub use vm::{VM, VMResult, VMError};
pub use store::Store;
pub use instr::{Instr, Literal, BuiltinOp};

// WASM-specific bindings
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
pub mod wasm_bindings {
    use wasm_bindgen::prelude::*;
    use crate::{parse, compile, vm, store};
    
    #[wasm_bindgen(start)]
    pub fn init() {
        console_error_panic_hook::set_once();
    }
    
    /// Evaluate RVM assembly code and return the result as a string
    #[wasm_bindgen]
    pub fn evaluate(source: &str) -> Result<String, JsValue> {
        let mut st = store::Store::new();
        let main_hash = parse::parse_file(source, &mut st)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        let mut machine = vm::VM::new(&st);
        let result = machine.run(main_hash)
            .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
        
        Ok(format!("{:?}", result))
    }
    
    /// Parse RVM assembly and return debug info as JSON
    #[wasm_bindgen]
    pub fn parse_rvm(source: &str) -> Result<String, JsValue> {
        let mut st = store::Store::new();
        let main_hash = parse::parse_file(source, &mut st)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        Ok(format!("{{\"status\": \"ok\", \"main_hash\": \"{:?}\"}}", main_hash))
    }
    
    /// Evaluate a simple expression (for playground)
    #[wasm_bindgen]
    pub fn evaluate_expr(source: &str) -> Result<String, JsValue> {
        let expr = parse_simple_expr(source)
            .map_err(|e| JsValue::from_str(&e))?;
        
        let block = compile::Compiler::compile(&expr);
        
        let mut st = store::Store::new();
        let hash = st.add_code(block);
        let mut machine = vm::VM::new(&st);
        
        let result = machine.run(hash)
            .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
        
        Ok(format!("{:?}", result))
    }
    
    /// Get version info
    #[wasm_bindgen]
    pub fn version() -> String {
        format!("RosettaVM {} (WASM)", env!("CARGO_PKG_VERSION"))
    }
    
    /// Run a simple calculation (for demos)
    #[wasm_bindgen]
    pub fn calculate(expr: &str) -> Result<String, JsValue> {
        let expr = parse_simple_expr(expr)
            .map_err(|e| JsValue::from_str(&e))?;
        
        let block = compile::Compiler::compile(&expr);
        
        let mut st = store::Store::new();
        let hash = st.add_code(block);
        let mut machine = vm::VM::new(&st);
        
        let result = machine.run(hash)
            .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
        
        Ok(format!("{:?}", result))
    }
    
    /// Parse a simple expression (for the playground)
    fn parse_simple_expr(source: &str) -> Result<compile::Expr, String> {
        let source = source.trim();
        
        // Try to parse as integer
        if let Ok(n) = source.parse::<i64>() {
            return Ok(compile::Expr::Int(n));
        }
        
        // Try to parse as boolean
        if source == "true" {
            return Ok(compile::Expr::Bool(true));
        }
        if source == "false" {
            return Ok(compile::Expr::Bool(false));
        }
        
        // Try simple binary expression: a + b, a * b, etc.
        for (op_str, op) in [
            (" + ", compile::BinOp::Add),
            (" - ", compile::BinOp::Sub),
            (" * ", compile::BinOp::Mul),
            (" / ", compile::BinOp::Div),
            (" % ", compile::BinOp::Mod),
            (" == ", compile::BinOp::Eq),
            (" != ", compile::BinOp::Ne),
            (" < ", compile::BinOp::Lt),
            (" <= ", compile::BinOp::Le),
            (" > ", compile::BinOp::Gt),
            (" >= ", compile::BinOp::Ge),
        ] {
            if let Some(pos) = source.find(op_str) {
                let left = parse_simple_expr(&source[..pos])?;
                let right = parse_simple_expr(&source[pos + op_str.len()..])?;
                return Ok(compile::Expr::BinOp(op, Box::new(left), Box::new(right)));
            }
        }
        
        // Try list: [a, b, c]
        if source.starts_with('[') && source.ends_with(']') {
            let inner = &source[1..source.len()-1];
            if inner.is_empty() {
                return Ok(compile::Expr::List(vec![]));
            }
            let items: Result<Vec<_>, _> = inner
                .split(',')
                .map(|s| parse_simple_expr(s.trim()))
                .collect();
            return Ok(compile::Expr::List(items?));
        }
        
        // String literal
        if source.starts_with('"') && source.ends_with('"') {
            return Ok(compile::Expr::Str(source[1..source.len()-1].to_string()));
        }
        
        // Variable
        if source.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Ok(compile::Expr::Var(source.to_string()));
        }
        
        Err(format!("Cannot parse expression: {}", source))
    }
    
    /// Compile Phi source code to RVM assembly using proper grammar-based parser
    #[wasm_bindgen]
    pub fn compile_phi(source: &str) -> Result<String, JsValue> {
        use crate::port::parser::parse_expr;
        
        // Parse using the proper grammar-based parser
        let expr = parse_expr(source)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        // Convert port::Expr to RVM assembly
        let rvm_code = port_expr_to_rvm(&expr);
        
        Ok(rvm_code)
    }
    
    /// Convert a port::Expr AST to RVM assembly string
    fn port_expr_to_rvm(expr: &crate::port::expr::Expr) -> String {
        use crate::port::expr::Expr;
        use crate::port::pattern::Pattern;
        
        struct Compiler {
            output: String,
            var_slots: std::collections::HashMap<String, u32>,
            next_slot: u32,
            label_counter: u32,
        }
        
        impl Compiler {
            fn new() -> Self {
                Compiler {
                    output: String::new(),
                    var_slots: std::collections::HashMap::new(),
                    next_slot: 0,
                    label_counter: 0,
                }
            }
            
            fn fresh_label(&mut self) -> u32 {
                self.label_counter += 1;
                self.label_counter
            }
            
            fn emit(&mut self, instr: &str) {
                self.output.push_str("  ");
                self.output.push_str(instr);
                self.output.push('\n');
            }
            
            fn compile_expr(&mut self, expr: &Expr) {
                match expr {
                    Expr::Int(n) => self.emit(&format!("PUSH {}", n)),
                    Expr::Bool(b) => self.emit(&format!("PUSH {}", if *b { "true" } else { "false" })),
                    Expr::String(s) => self.emit(&format!("PUSH \"{}\"", s)),
                    Expr::Unit => self.emit("PUSH ()"),
                    
                    Expr::Var(name) => {
                        if let Some(&slot) = self.var_slots.get(name) {
                            self.emit(&format!("LOAD {}", slot));
                        } else {
                            self.emit(&format!("GLOBAL {}", name));
                        }
                    }
                    
                    Expr::BinOp(op, left, right) => {
                        self.compile_expr(left);
                        self.compile_expr(right);
                        let instr = match op.as_str() {
                            "+" => "ADD", "-" => "SUB", "*" => "MUL", "/" => "DIV", "%" => "MOD",
                            "==" => "EQ", "!=" => "NE", "<" => "LT", "<=" => "LE", ">" => "GT", ">=" => "GE",
                            "&&" => "AND", "||" => "OR", "++" => "APPEND",
                            _ => "NOP",
                        };
                        self.emit(instr);
                    }
                    
                    Expr::If(cond, then_branch, else_branch) => {
                        let else_label = self.fresh_label();
                        let end_label = self.fresh_label();
                        
                        self.compile_expr(cond);
                        self.emit(&format!("JUMPIFNOT L{}", else_label));
                        self.compile_expr(then_branch);
                        self.emit(&format!("JUMP L{}", end_label));
                        self.output.push_str(&format!("L{}:\n", else_label));
                        self.compile_expr(else_branch);
                        self.output.push_str(&format!("L{}:\n", end_label));
                    }
                    
                    Expr::Lam(param, body) => {
                        let func_label = self.fresh_label();
                        let after_label = self.fresh_label();
                        
                        self.emit(&format!("CLOSURE F{}", func_label));
                        self.emit(&format!("JUMP L{}", after_label));
                        
                        self.output.push_str(&format!("F{}:\n", func_label));
                        let slot = self.next_slot;
                        self.var_slots.insert(param.clone(), slot);
                        self.next_slot += 1;
                        self.emit(&format!("STORE {}", slot));
                        self.compile_expr(body);
                        self.emit("RETURN");
                        
                        self.output.push_str(&format!("L{}:\n", after_label));
                    }
                    
                    Expr::App(func, arg) => {
                        self.compile_expr(arg);
                        self.compile_expr(func);
                        self.emit("APPLY");
                    }
                    
                    Expr::Let(name, value, body) => {
                        self.compile_expr(value);
                        let slot = self.next_slot;
                        self.var_slots.insert(name.clone(), slot);
                        self.next_slot += 1;
                        self.emit(&format!("STORE {}", slot));
                        self.compile_expr(body);
                    }
                    
                    Expr::LetRec(name, value, body) => {
                        let slot = self.next_slot;
                        self.var_slots.insert(name.clone(), slot);
                        self.next_slot += 1;
                        self.emit("PUSH ()");
                        self.emit(&format!("STORE {}", slot));
                        self.compile_expr(value);
                        self.emit(&format!("STORE {}", slot));
                        self.compile_expr(body);
                    }
                    
                    Expr::List(elems) => {
                        self.emit("PUSH NIL");
                        for elem in elems.iter().rev() {
                            self.compile_expr(elem);
                            self.emit("SWAP");
                            self.emit("CONS");
                        }
                    }
                    
                    Expr::Cons(head, tail) => {
                        self.compile_expr(tail);
                        self.compile_expr(head);
                        self.emit("CONS");
                    }
                    
                    Expr::Tuple(elems) => {
                        for elem in elems {
                            self.compile_expr(elem);
                        }
                        self.emit(&format!("MKTUPLE {}", elems.len()));
                    }
                    
                    Expr::Proj(tuple, idx) => {
                        self.compile_expr(tuple);
                        self.emit(&format!("GETFIELD {}", idx));
                    }
                    
                    Expr::Ctor(name, args) => {
                        for arg in args {
                            self.compile_expr(arg);
                        }
                        self.emit(&format!("MKCTOR {} {}", name, args.len()));
                    }
                    
                    Expr::Match(scrutinee, cases) => {
                        self.compile_expr(scrutinee);
                        let end_label = self.fresh_label();
                        
                        for case in cases {
                            let next_case = self.fresh_label();
                            self.emit("DUP");
                            self.compile_pattern_test(&case.pattern, next_case);
                            self.emit("POP");
                            self.compile_expr(&case.body);
                            self.emit(&format!("JUMP L{}", end_label));
                            self.output.push_str(&format!("L{}:\n", next_case));
                        }
                        
                        self.emit("PUSH \"match failure\"");
                        self.emit("HALT");
                        self.output.push_str(&format!("L{}:\n", end_label));
                    }
                    
                    Expr::Builtin(name) => {
                        self.emit(&format!("BUILTIN {}", name));
                    }
                }
            }
            
            fn compile_pattern_test(&mut self, pattern: &Pattern, fail_label: u32) {
                use crate::port::pattern::Literal;
                match pattern {
                    Pattern::Wild => {}
                    Pattern::Var(name) => {
                        let slot = self.next_slot;
                        self.var_slots.insert(name.clone(), slot);
                        self.next_slot += 1;
                        self.emit("DUP");
                        self.emit(&format!("STORE {}", slot));
                    }
                    Pattern::Lit(Literal::Int(n)) => {
                        self.emit("DUP");
                        self.emit(&format!("PUSH {}", n));
                        self.emit("EQ");
                        self.emit(&format!("JUMPIFNOT L{}", fail_label));
                    }
                    Pattern::Lit(Literal::Bool(b)) => {
                        self.emit("DUP");
                        self.emit(&format!("PUSH {}", if *b { "true" } else { "false" }));
                        self.emit("EQ");
                        self.emit(&format!("JUMPIFNOT L{}", fail_label));
                    }
                    Pattern::Lit(Literal::String(s)) => {
                        self.emit("DUP");
                        self.emit(&format!("PUSH \"{}\"", s));
                        self.emit("EQ");
                        self.emit(&format!("JUMPIFNOT L{}", fail_label));
                    }
                    Pattern::Ctor(name, pats) => {
                        self.emit("DUP");
                        self.emit(&format!("TESTTAG {}", name));
                        self.emit(&format!("JUMPIFNOT L{}", fail_label));
                        for (i, pat) in pats.iter().enumerate() {
                            self.emit("DUP");
                            self.emit(&format!("GETFIELD {}", i));
                            self.compile_pattern_test(pat, fail_label);
                            self.emit("POP");
                        }
                    }
                    Pattern::Cons(head, tail) => {
                        self.emit("DUP");
                        self.emit("ISNIL");
                        self.emit(&format!("JUMPIF L{}", fail_label));
                        self.emit("DUP");
                        self.emit("HEAD");
                        self.compile_pattern_test(head, fail_label);
                        self.emit("POP");
                        self.emit("DUP");
                        self.emit("TAIL");
                        self.compile_pattern_test(tail, fail_label);
                        self.emit("POP");
                    }
                    Pattern::List(pats) => {
                        for pat in pats {
                            self.emit("DUP");
                            self.emit("ISNIL");
                            self.emit(&format!("JUMPIF L{}", fail_label));
                            self.emit("DUP");
                            self.emit("HEAD");
                            self.compile_pattern_test(pat, fail_label);
                            self.emit("POP");
                            self.emit("TAIL");
                        }
                        self.emit("DUP");
                        self.emit("ISNIL");
                        self.emit(&format!("JUMPIFNOT L{}", fail_label));
                    }
                    Pattern::Tuple(pats) => {
                        for (i, pat) in pats.iter().enumerate() {
                            self.emit("DUP");
                            self.emit(&format!("GETFIELD {}", i));
                            self.compile_pattern_test(pat, fail_label);
                            self.emit("POP");
                        }
                    }
                    Pattern::As(name, inner) => {
                        let slot = self.next_slot;
                        self.var_slots.insert(name.clone(), slot);
                        self.next_slot += 1;
                        self.emit("DUP");
                        self.emit(&format!("STORE {}", slot));
                        self.compile_pattern_test(inner, fail_label);
                    }
                }
            }
        }
        
        let mut compiler = Compiler::new();
        compiler.output.push_str("@main\n");
        compiler.compile_expr(expr);
        compiler.emit("HALT");
        compiler.output
    }
}

// Unit tests
#[cfg(test)]
mod tests {
    use super::compile;
    
    fn parse_simple_expr(source: &str) -> Result<compile::Expr, String> {
        let source = source.trim();
        if let Ok(n) = source.parse::<i64>() {
            return Ok(compile::Expr::Int(n));
        }
        if source == "true" {
            return Ok(compile::Expr::Bool(true));
        }
        for (op_str, op) in [(" + ", compile::BinOp::Add)] {
            if let Some(pos) = source.find(op_str) {
                let left = parse_simple_expr(&source[..pos])?;
                let right = parse_simple_expr(&source[pos + op_str.len()..])?;
                return Ok(compile::Expr::BinOp(op, Box::new(left), Box::new(right)));
            }
        }
        Err(format!("Cannot parse: {}", source))
    }
    
    #[test]
    fn test_parse_int() {
        let expr = parse_simple_expr("42").unwrap();
        assert!(matches!(expr, compile::Expr::Int(42)));
    }
    
    #[test]
    fn test_parse_add() {
        let expr = parse_simple_expr("1 + 2").unwrap();
        assert!(matches!(expr, compile::Expr::BinOp(compile::BinOp::Add, _, _)));
    }
}
