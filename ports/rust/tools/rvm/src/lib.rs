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
    /// For full programs with type signatures and function definitions, extracts main expression
    #[wasm_bindgen]
    pub fn compile_phi(source: &str) -> Result<String, JsValue> {
        use crate::port::parser::parse_expr;
        
        // Check if this looks like a full program (has type signatures or function defs)
        let is_full_program = source.lines().any(|line| {
            let line = line.trim();
            !line.is_empty() && !line.starts_with("--") && (
                line.contains(" : ") ||  // Type signature
                (line.starts_with("main") && line.contains("=")) ||  // main definition
                line.contains(" = ") && !line.starts_with("let")  // Function definition
            )
        });
        
        if is_full_program {
            // For full programs, compile using the program parser
            return compile_phi_program(source);
        }
        
        // For simple expressions, use the expression parser
        let expr = parse_expr(source)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        // Convert port::Expr to RVM assembly using RvmCompiler
        let mut compiler = RvmCompiler::new();
        compiler.output.push_str("@main\n");
        compiler.compile_expr(&expr);
        compiler.emit("HALT");
        
        Ok(compiler.output)
    }
    
    /// Compile a full Phi program with type signatures and function definitions
    fn compile_phi_program(source: &str) -> Result<String, JsValue> {
        use crate::port::parser::parse_expr;
        use std::collections::HashMap;
        
        let mut rvm = String::from("; Generated by phi2rvm compiler (WASM)\n");
        rvm.push_str("; Source: main.phi\n\n");
        
        // Parse the program: collect type signatures, function definitions, and main
        let mut function_cases: HashMap<String, Vec<(String, String)>> = HashMap::new();  // name -> [(args, body)]
        let mut main_expr: Option<String> = None;
        
        for line in source.lines() {
            let line = line.trim();
            
            // Skip empty lines and comments
            if line.is_empty() || line.starts_with("--") {
                continue;
            }
            
            // Skip type declarations (type X = ..., data X = ...)
            if line.starts_with("type ") || line.starts_with("data ") {
                continue;
            }
            
            // Skip type signatures (name : Type)
            if line.contains(" : ") && !line.contains(" = ") {
                continue;
            }
            
            // Main expression
            if line.starts_with("main") && line.contains("=") {
                if let Some(pos) = line.find('=') {
                    main_expr = Some(line[pos + 1..].trim().to_string());
                }
                continue;
            }
            
            // Function definition: name args = body
            if line.contains(" = ") {
                if let Some((lhs, body)) = line.split_once('=') {
                    let lhs = lhs.trim();
                    let body = body.trim();
                    let parts: Vec<&str> = lhs.split_whitespace().collect();
                    if !parts.is_empty() {
                        let name = parts[0];
                        let args = parts[1..].join(" ");
                        function_cases.entry(name.to_string())
                            .or_insert_with(Vec::new)
                            .push((args, body.to_string()));
                    }
                }
            }
        }
        
        // Generate RVM for each function
        for (name, cases) in &function_cases {
            rvm.push_str(&compile_function_to_rvm(name, cases)?);
        }
        
        // Generate main
        if let Some(expr) = main_expr {
            rvm.push_str("fn main() {\n");
            
            // Try to parse and compile the main expression
            match parse_expr(&expr) {
                Ok(parsed_expr) => {
                    let mut compiler = RvmCompiler::new();
                    compiler.compile_expr(&parsed_expr);
                    for line in compiler.output.lines() {
                        rvm.push_str("  ");
                        rvm.push_str(line);
                        rvm.push('\n');
                    }
                }
                Err(_) => {
                    // Fallback: simple expression compilation
                    rvm.push_str(&format!("  ; {}\n", expr));
                    compile_simple_expr(&expr, &mut rvm, &function_cases);
                }
            }
            
            rvm.push_str("  HALT\n");
            rvm.push_str("}\n");
        }
        
        Ok(rvm)
    }
    
    /// Compile a single function with pattern matching cases
    fn compile_function_to_rvm(name: &str, cases: &[(String, String)]) -> Result<String, JsValue> {
        use crate::port::parser::parse_expr;
        
        // Find the general parameter name (non-numeric pattern)
        let param_name = cases.iter()
            .find(|(args, _)| !args.chars().all(|c| c.is_ascii_digit()))
            .map(|(args, _)| args.as_str())
            .unwrap_or("n");
        
        let mut rvm = format!("fn {}({}) {{\n", name, param_name);
        
        // If there's only one case with a variable pattern, emit directly
        if cases.len() == 1 {
            let (args, body) = &cases[0];
            if !args.chars().all(|c| c.is_ascii_digit()) {
                match parse_expr(body) {
                    Ok(expr) => {
                        let mut compiler = RvmCompiler::new();
                        compiler.var_slots.insert(args.clone(), 0);
                        compiler.next_slot = 1;
                        compiler.compile_expr(&expr);
                        for line in compiler.output.lines() {
                            rvm.push_str("    ");
                            rvm.push_str(line);
                            rvm.push('\n');
                        }
                    }
                    Err(_) => {
                        rvm.push_str(&format!("    ; {}\n", body));
                    }
                }
                rvm.push_str("    RETURN\n");
                rvm.push_str("}\n\n");
                return Ok(rvm);
            }
        }
        
        // Multiple cases with pattern matching
        let mut base_case = None;
        let mut pattern_cases: Vec<_> = vec![];
        
        for (args, body) in cases {
            if args.chars().all(|c| c.is_ascii_digit()) {
                pattern_cases.push((args.parse::<i64>().unwrap_or(0), body.clone()));
            } else {
                base_case = Some(body.clone());
            }
        }
        
        // Sort pattern cases
        pattern_cases.sort_by_key(|(n, _)| *n);
        
        // Emit pattern matching
        for (i, (pattern_val, body)) in pattern_cases.iter().enumerate() {
            let label = format!("L{}_{}", name, i);
            rvm.push_str(&format!("    LOAD 0\n"));
            rvm.push_str(&format!("    PUSH {}\n", pattern_val));
            rvm.push_str(&format!("    EQ\n"));
            rvm.push_str(&format!("    JUMPIFNOT {}\n", label));
            
            match parse_expr(body) {
                Ok(expr) => {
                    let mut compiler = RvmCompiler::new();
                    compiler.var_slots.insert(param_name.to_string(), 0);
                    compiler.next_slot = 1;
                    compiler.compile_expr(&expr);
                    for line in compiler.output.lines() {
                        rvm.push_str("    ");
                        rvm.push_str(line);
                        rvm.push('\n');
                    }
                }
                Err(_) => {
                    rvm.push_str(&format!("    ; {}\n", body));
                }
            }
            
            rvm.push_str(&format!("    RETURN\n"));
            rvm.push_str(&format!("{}:\n", label));
        }
        
        // Base case
        if let Some(body) = base_case {
            match parse_expr(&body) {
                Ok(expr) => {
                    let mut compiler = RvmCompiler::new();
                    compiler.var_slots.insert(param_name.to_string(), 0);
                    compiler.next_slot = 1;
                    compiler.compile_expr(&expr);
                    for line in compiler.output.lines() {
                        rvm.push_str("    ");
                        rvm.push_str(line);
                        rvm.push('\n');
                    }
                }
                Err(_) => {
                    rvm.push_str(&format!("    ; {}\n", body));
                }
            }
        }
        
        rvm.push_str("    RETURN\n");
        rvm.push_str("}\n\n");
        
        Ok(rvm)
    }
    
    /// Simple expression compiler for fallback
    fn compile_simple_expr(expr: &str, rvm: &mut String, functions: &std::collections::HashMap<String, Vec<(String, String)>>) {
        let expr = expr.trim();
        
        // Function call: fname arg
        let parts: Vec<&str> = expr.split_whitespace().collect();
        if parts.len() >= 2 {
            if functions.contains_key(parts[0]) {
                // Compile argument
                let arg = parts[1..].join(" ");
                if arg.starts_with('"') && arg.ends_with('"') {
                    rvm.push_str(&format!("  PUSH {}\n", arg));
                } else if let Ok(n) = arg.parse::<i64>() {
                    rvm.push_str(&format!("  PUSH {}\n", n));
                } else {
                    rvm.push_str(&format!("  PUSH \"{}\"\n", arg));
                }
                rvm.push_str(&format!("  CALL {}\n", parts[0]));
                return;
            }
        }
        
        // String literal
        if expr.starts_with('"') && expr.ends_with('"') {
            rvm.push_str(&format!("  PUSH {}\n", expr));
            return;
        }
        
        // Number
        if let Ok(n) = expr.parse::<i64>() {
            rvm.push_str(&format!("  PUSH {}\n", n));
            return;
        }
        
        rvm.push_str(&format!("  ; TODO: {}\n", expr));
    }
    
    /// Internal compiler state for port::Expr to RVM
    struct RvmCompiler {
        output: String,
        var_slots: std::collections::HashMap<String, u32>,
        next_slot: u32,
        label_counter: u32,
    }
    
    impl RvmCompiler {
        fn new() -> Self {
            RvmCompiler {
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
            self.output.push_str(instr);
            self.output.push('\n');
        }
        
        fn compile_expr(&mut self, expr: &crate::port::expr::Expr) {
            use crate::port::expr::Expr;
            
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
                    use crate::port::pattern::{Pattern, Literal};
                    
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
        
        fn compile_pattern_test(&mut self, pattern: &crate::port::pattern::Pattern, fail_label: u32) {
            use crate::port::pattern::{Pattern, Literal};
            
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
