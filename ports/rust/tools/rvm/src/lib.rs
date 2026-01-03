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
pub mod print;
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

// WASM-specific bindings - functions must be at crate root for wasm-bindgen export
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
use wasm_bindgen::prelude::*;

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
#[wasm_bindgen(start)]
pub fn init() {
    console_error_panic_hook::set_once();
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
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

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Parse RVM assembly and return debug info as JSON
#[wasm_bindgen]
pub fn parse_rvm(source: &str) -> Result<String, JsValue> {
    let mut st = store::Store::new();
    let main_hash = parse::parse_file(source, &mut st)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    
    Ok(format!("{{\"status\": \"ok\", \"main_hash\": \"{:?}\"}}", main_hash))
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Get version info
#[wasm_bindgen]
pub fn version() -> String {
    format!("RosettaVM {} (WASM)", env!("CARGO_PKG_VERSION"))
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Run a simple calculation (for demos)
#[wasm_bindgen]
pub fn calculate(expr: &str) -> Result<String, JsValue> {
    let expr = wasm_parse_simple_expr(expr)
        .map_err(|e| JsValue::from_str(&e))?;
    
    let block = compile::Compiler::compile(&expr);
    
    let mut st = store::Store::new();
    let hash = st.add_code(block);
    let mut machine = vm::VM::new(&st);
    
    let result = machine.run(hash)
        .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
    
    Ok(format!("{:?}", result))
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Evaluate a simple expression (for playground)
#[wasm_bindgen]
pub fn evaluate_expr(source: &str) -> Result<String, JsValue> {
    let expr = wasm_parse_simple_expr(source)
        .map_err(|e| JsValue::from_str(&e))?;
    
    let block = compile::Compiler::compile(&expr);
    
    let mut st = store::Store::new();
    let hash = st.add_code(block);
    let mut machine = vm::VM::new(&st);
    
    let result = machine.run(hash)
        .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
    
    Ok(format!("{:?}", result))
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Compile Phi source to RVM assembly (for display only)
#[wasm_bindgen]
pub fn compile_phi(source: &str) -> Result<String, JsValue> {
    // Check if this looks like a full program (has main =)
    let is_program = source.lines()
        .any(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("main") && trimmed.contains("=")
        });
    
    if is_program {
        compile_phi_program(source)
    } else {
        compile_phi_expr(source)
    }
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
/// Compile and run Phi source directly (bypasses text RVM)
#[wasm_bindgen]
pub fn run_phi(source: &str) -> Result<String, JsValue> {
    use crate::port::parser::Parser;
    use std::collections::HashMap;
    
    let mut st = store::Store::new();
    let mut function_names: HashMap<String, hash::Hash> = HashMap::new();
    
    // Check if this looks like a full program (has main =)
    let is_program = source.lines()
        .any(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("main") && trimmed.contains("=")
        });
    
    if is_program {
        // First pass: collect and compile function definitions
        let mut main_body = None;
        
        for line in source.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("--") {
                continue;
            }
            if line.starts_with("type ") || line.starts_with("data ") {
                continue;
            }
            if line.contains(" : ") && !line.contains(" = ") {
                continue;
            }
            
            // Main expression
            if line.starts_with("main") && line.contains("=") {
                if let Some(pos) = line.find('=') {
                    main_body = Some(line[pos + 1..].trim().to_string());
                }
                continue;
            }
            
            // Function definition: name args = body (simple, no pattern matching)
            if line.contains(" = ") {
                if let Some((lhs, body)) = line.split_once('=') {
                    let lhs = lhs.trim();
                    let body = body.trim();
                    let parts: Vec<&str> = lhs.split_whitespace().collect();
                    if parts.is_empty() {
                        continue;
                    }
                    
                    let name = parts[0];
                    let args = &parts[1..];
                    
                    // Skip pattern matching (args with non-identifier chars)
                    let has_patterns = args.iter().any(|arg| {
                        !arg.chars().all(|c| c.is_alphanumeric() || c == '_') ||
                        arg.chars().next().map(|c| c.is_numeric()).unwrap_or(false) ||
                        *arg == "_"
                    });
                    if has_patterns {
                        continue;
                    }
                    
                    // Parse body and compile as function
                    if let Ok(mut parser) = Parser::new(body) {
                        if let Ok(port_expr) = parser.parse_expr() {
                            let expr = convert_expr(&port_expr);
                            // Compile with parameters as env slots
                            let params: Vec<String> = args.iter().map(|s| s.to_string()).collect();
                            let mut block = compile::Compiler::compile_function(&params, &expr);
                            block.name = Some(name.to_string());
                            let code_hash = st.add_code(block);
                            
                            // Register as a closure VALUE at the name hash
                            // This allows LoadGlobal(Hash::of_str(name)) to find it
                            let name_hash = hash::Hash::of_str(name);
                            let closure_val = value::Val::closure(code_hash, value::Env::new());
                            st.set_value(name_hash, closure_val);
                            
                            function_names.insert(name.to_string(), code_hash);
                        }
                    }
                }
            }
        }
        
        // Now compile main with function references available
        let main_body = main_body.ok_or_else(|| JsValue::from_str("No main expression found"))?;
        
        let mut parser = Parser::new(&main_body)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        let port_expr = parser.parse_expr()
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        let expr = convert_expr(&port_expr);
        let mut block = compile::Compiler::compile(&expr);
        block.name = Some("main".to_string());
        let main_hash = st.add_code(block);
        
        let mut machine = vm::VM::new(&st);
        let result = machine.run(main_hash)
            .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
        
        Ok(format!("{:?}", result))
    } else {
        // Simple expression mode
        let mut parser = Parser::new(source)
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        let port_expr = parser.parse_expr()
            .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
        
        let expr = convert_expr(&port_expr);
        let block = compile::Compiler::compile(&expr);
        let hash = st.add_code(block);
        
        let mut machine = vm::VM::new(&st);
        let result = machine.run(hash)
            .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
        
        Ok(format!("{:?}", result))
    }
}

// Helper function to convert port::expr::Expr to compile::Expr
// Available always (used by tests and WASM)
fn convert_expr(e: &crate::port::expr::Expr) -> compile::Expr {
    use crate::port::expr::Expr as PortExpr;
    match e {
        PortExpr::Int(n) => compile::Expr::Int(*n),
        PortExpr::String(s) => compile::Expr::Str((**s).clone()),
        PortExpr::Bool(b) => compile::Expr::Bool(*b),
        PortExpr::Unit => compile::Expr::Tuple(vec![]),
        PortExpr::Var(name) => compile::Expr::Var(name.clone()),
        PortExpr::Ctor(name, args) => {
            // Convert constructor to a function application
            // Ctor("Yoneda", [arg]) -> App(Var("Yoneda"), arg)
            let ctor_fn = compile::Expr::Var(name.clone());
            if args.is_empty() {
                ctor_fn
            } else {
                let converted_args: Vec<compile::Expr> = args.iter().map(convert_expr).collect();
                compile::Expr::App(Box::new(ctor_fn), converted_args)
            }
        }
        PortExpr::Lam(param, body) => {
            compile::Expr::Lambda(vec![param.clone()], Box::new(convert_expr(body)))
        }
        PortExpr::App(f, arg) => {
            compile::Expr::App(Box::new(convert_expr(f)), vec![convert_expr(arg)])
        }
        PortExpr::Let(name, val, body) => {
            compile::Expr::Let(name.clone(), Box::new(convert_expr(val)), Box::new(convert_expr(body)))
        }
        PortExpr::LetRec(name, val, body) => {
            // For now, treat LetRec same as Let
            compile::Expr::Let(name.clone(), Box::new(convert_expr(val)), Box::new(convert_expr(body)))
        }
        PortExpr::If(cond, then_e, else_e) => {
            compile::Expr::If(Box::new(convert_expr(cond)), Box::new(convert_expr(then_e)), Box::new(convert_expr(else_e)))
        }
        PortExpr::BinOp(op, l, r) => {
            let bin_op = match op.as_str() {
                "+" => compile::BinOp::Add,
                "-" => compile::BinOp::Sub,
                "*" => compile::BinOp::Mul,
                "/" => compile::BinOp::Div,
                "%" => compile::BinOp::Mod,
                "==" => compile::BinOp::Eq,
                "!=" => compile::BinOp::Ne,
                "<" => compile::BinOp::Lt,
                "<=" => compile::BinOp::Le,
                ">" => compile::BinOp::Gt,
                ">=" => compile::BinOp::Ge,
                "&&" => compile::BinOp::And,
                "||" => compile::BinOp::Or,
                "++" => compile::BinOp::Concat,
                ":" => compile::BinOp::Cons,
                _ => compile::BinOp::Add, // fallback
            };
            compile::Expr::BinOp(bin_op, Box::new(convert_expr(l)), Box::new(convert_expr(r)))
        }
        PortExpr::List(items) => {
            compile::Expr::List(items.iter().map(convert_expr).collect())
        }
        PortExpr::Tuple(items) => {
            compile::Expr::Tuple(items.iter().map(convert_expr).collect())
        }
        _ => compile::Expr::Int(0), // fallback for unhandled cases
    }
}

// Helper functions for WASM (not exported)
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn wasm_parse_simple_expr(source: &str) -> Result<compile::Expr, String> {
    use crate::port::parser::Parser;
    let mut parser = Parser::new(source).map_err(|e| format!("{}", e))?;
    let port_expr = parser.parse_expr().map_err(|e| format!("{}", e))?;
    Ok(convert_expr(&port_expr))
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn compile_phi_expr(source: &str) -> Result<String, JsValue> {
    use crate::port::parser::Parser;
    use crate::print::{PrintContext, print_instr};
    
    let mut parser = Parser::new(source)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    let port_expr = parser.parse_expr()
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    
    // Convert to compile::Expr and compile
    let expr = convert_expr(&port_expr);
    let block = compile::Compiler::compile(&expr);
    
    // Format as RVM assembly using print module
    let ctx = PrintContext::new();
    let mut output = String::new();
    output.push_str("fn main() {\n");
    for instr in &block.code {
        output.push_str(&format!("    {}\n", print_instr(instr, &ctx)));
    }
    output.push_str("}\n");
    
    Ok(output)
}

/// Pattern in function arguments
#[derive(Debug, Clone)]
enum FnPattern {
    /// Simple variable binding: x
    Var(String),
    /// Constructor pattern: (Ctor x y z)
    Ctor(String, Vec<String>),
    /// Literal pattern: 0, "hello"
    Literal(String),
}

/// Parse function argument patterns
/// Handles: x, (Ctor x y), 0, "str"
fn parse_function_patterns(args: &str) -> Result<Vec<FnPattern>, String> {
    let mut patterns = Vec::new();
    let mut chars = args.chars().peekable();
    
    while let Some(&c) = chars.peek() {
        // Skip whitespace
        if c.is_whitespace() {
            chars.next();
            continue;
        }
        
        if c == '(' {
            // Constructor pattern: (Ctor x y z)
            chars.next(); // consume '('
            let mut inner = String::new();
            let mut depth = 1;
            while let Some(ch) = chars.next() {
                if ch == '(' { depth += 1; }
                if ch == ')' { 
                    depth -= 1;
                    if depth == 0 { break; }
                }
                inner.push(ch);
            }
            // Parse inner as "Ctor x y z"
            let parts: Vec<&str> = inner.split_whitespace().collect();
            if parts.is_empty() {
                return Err("Empty constructor pattern".to_string());
            }
            let ctor_name = parts[0].to_string();
            let bindings: Vec<String> = parts[1..].iter().map(|s| s.to_string()).collect();
            patterns.push(FnPattern::Ctor(ctor_name, bindings));
        } else if c.is_alphabetic() || c == '_' {
            // Variable or constructor without parens
            let mut name = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_alphanumeric() || ch == '_' || ch == '\'' {
                    name.push(ch);
                    chars.next();
                } else {
                    break;
                }
            }
            // Check if uppercase (constructor) or lowercase (variable)
            if name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                patterns.push(FnPattern::Ctor(name, vec![]));
            } else {
                patterns.push(FnPattern::Var(name));
            }
        } else if c.is_numeric() {
            // Literal number pattern
            let mut num = String::new();
            while let Some(&ch) = chars.peek() {
                if ch.is_numeric() {
                    num.push(ch);
                    chars.next();
                } else {
                    break;
                }
            }
            patterns.push(FnPattern::Literal(num));
        } else if c == '"' {
            // String literal pattern
            chars.next(); // consume opening quote
            let mut s = String::from("\"");
            while let Some(ch) = chars.next() {
                s.push(ch);
                if ch == '"' { break; }
            }
            patterns.push(FnPattern::Literal(s));
        } else {
            chars.next(); // skip unknown
        }
    }
    
    Ok(patterns)
}

/// Compile a function with pattern matching
fn compile_with_patterns(params: &[String], patterns: &[FnPattern], body: &compile::Expr) -> instr::CodeBlock {
    use crate::instr::{Instr, Literal, CodeBlock};
    
    let mut code: Vec<Instr> = Vec::new();
    let num_params = params.len();
    
    // For each constructor pattern, generate field extraction code
    for (idx, pattern) in patterns.iter().enumerate() {
        if let FnPattern::Ctor(_ctor_name, bindings) = pattern {
            // The argument is at env slot `idx`
            // For each binding, extract the field
            for (field_idx, _binding) in bindings.iter().enumerate() {
                // Load the argument from env
                code.push(Instr::Load(idx as u32));
                // Get field at index
                code.push(Instr::GetField(field_idx as u8));
                // Store it in a new slot (after all params)
                code.push(Instr::Store((num_params + idx * 10 + field_idx) as u32));
            }
        }
    }
    
    // Now compile the body with pattern bindings available
    // Create a modified compiler that knows about pattern bindings
    let mut locals: Vec<(String, u32)> = Vec::new();
    
    // Add original params
    for (i, param) in params.iter().enumerate() {
        locals.push((param.clone(), i as u32));
    }
    
    // Add pattern bindings
    for (idx, pattern) in patterns.iter().enumerate() {
        if let FnPattern::Ctor(_ctor_name, bindings) = pattern {
            for (field_idx, binding) in bindings.iter().enumerate() {
                if binding != "_" {
                    let slot = (num_params + idx * 10 + field_idx) as u32;
                    locals.push((binding.clone(), slot));
                }
            }
        }
    }
    
    // Compile body with custom locals
    let body_code = compile_expr_with_locals(body, &locals);
    code.extend(body_code);
    code.push(Instr::Return);
    
    CodeBlock::new(code)
}

/// Compile an expression with given local variable mappings
fn compile_expr_with_locals(expr: &compile::Expr, locals: &[(String, u32)]) -> Vec<Instr> {
    use crate::instr::{Instr, Literal};
    
    let mut code = Vec::new();
    
    match expr {
        compile::Expr::Int(n) => {
            code.push(Instr::Push(Literal::Int(*n)));
        }
        compile::Expr::Str(s) => {
            code.push(Instr::Push(Literal::Str(s.clone())));
        }
        compile::Expr::Bool(b) => {
            code.push(Instr::Push(Literal::Bool(*b)));
        }
        compile::Expr::Var(name) => {
            // Look up in locals
            if let Some((_, slot)) = locals.iter().find(|(n, _)| n == name) {
                code.push(Instr::Load(*slot));
            } else {
                // Global lookup
                code.push(Instr::LoadGlobal(Hash::of_str(name)));
            }
        }
        compile::Expr::BinOp(op, lhs, rhs) => {
            code.extend(compile_expr_with_locals(lhs, locals));
            code.extend(compile_expr_with_locals(rhs, locals));
            code.push(match op {
                compile::BinOp::Add => Instr::Add,
                compile::BinOp::Sub => Instr::Sub,
                compile::BinOp::Mul => Instr::Mul,
                compile::BinOp::Div => Instr::Div,
                compile::BinOp::Mod => Instr::Mod,
                compile::BinOp::Eq => Instr::Eq,
                compile::BinOp::Ne => Instr::Ne,
                compile::BinOp::Lt => Instr::Lt,
                compile::BinOp::Le => Instr::Le,
                compile::BinOp::Gt => Instr::Gt,
                compile::BinOp::Ge => Instr::Ge,
                compile::BinOp::And => Instr::And,
                compile::BinOp::Or => Instr::Or,
                compile::BinOp::Cons => Instr::Cons,
                compile::BinOp::Concat => Instr::StrConcat,
            });
        }
        compile::Expr::App(func, args) => {
            for arg in args {
                code.extend(compile_expr_with_locals(arg, locals));
            }
            code.extend(compile_expr_with_locals(func, locals));
            if args.len() == 1 {
                code.push(Instr::Apply);
            } else {
                code.push(Instr::ApplyN(args.len() as u8));
            }
        }
        compile::Expr::Lambda(params, body) => {
            // For simplicity, compile lambda as a closure
            // This is a simplification - proper implementation would create a code block
            let mut new_locals = locals.to_vec();
            for (i, param) in params.iter().enumerate() {
                new_locals.push((param.clone(), (locals.len() + i) as u32));
            }
            code.extend(compile_expr_with_locals(body, &new_locals));
        }
        compile::Expr::List(elems) => {
            for elem in elems {
                code.extend(compile_expr_with_locals(elem, locals));
            }
            code.push(Instr::MkList(elems.len() as u16));
        }
        compile::Expr::If(cond, then_e, else_e) => {
            code.extend(compile_expr_with_locals(cond, locals));
            let mut then_code = compile_expr_with_locals(then_e, locals);
            let mut else_code = compile_expr_with_locals(else_e, locals);
            
            // Jump over then branch if condition is false
            let jump_to_else = (then_code.len() + 2) as i32; // +1 for the jump at end of then
            code.push(Instr::JumpIfNot(jump_to_else));
            code.extend(then_code);
            let jump_to_end = (else_code.len() + 1) as i32;
            code.push(Instr::Jump(jump_to_end));
            code.extend(else_code);
        }
        compile::Expr::Let(name, val, body) => {
            code.extend(compile_expr_with_locals(val, locals));
            let new_slot = locals.len() as u32;
            code.push(Instr::Store(new_slot));
            let mut new_locals = locals.to_vec();
            new_locals.push((name.clone(), new_slot));
            code.extend(compile_expr_with_locals(body, &new_locals));
        }
        _ => {
            // Fallback: compile using default compiler
            let block = compile::Compiler::compile(expr);
            code.extend(block.code.into_iter().filter(|i| !matches!(i, Instr::Halt)));
        }
    }
    
    code
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn compile_phi_program(source: &str) -> Result<String, JsValue> {
    use crate::port::parser::Parser;
    use crate::print::{PrintContext, print_instr};
    
    let mut output = String::new();
    let mut function_cases: std::collections::HashMap<String, Vec<(String, String)>> = std::collections::HashMap::new();
    let mut main_expr: Option<String> = None;
    
    // PrintContext for name resolution
    let mut ctx = PrintContext::new();
    
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
    
    // Register all function names in PrintContext
    for (name, _) in &function_cases {
        ctx.register(name);
    }
    
    // Generate RVM for each function
    for (name, cases) in &function_cases {
        for (i, (args, body)) in cases.iter().enumerate() {
            // Try to parse pattern matching in args
            let pattern_result = parse_function_patterns(args);
            
            let fn_name = if cases.len() > 1 {
                format!("{}_{}", name, i)
            } else {
                name.clone()
            };
            
            match pattern_result {
                Ok(patterns) => {
                    // Parse and compile body
                    match Parser::new(body) {
                        Ok(mut parser) => match parser.parse_expr() {
                            Ok(port_expr) => {
                                // Convert body expression
                                let mut body_expr = convert_expr(&port_expr);
                                
                                // Wrap body with pattern extractions
                                // For each pattern like (Ctor x y), we generate:
                                //   let x = getfield 0 arg in let y = getfield 1 arg in body
                                let mut param_names: Vec<String> = Vec::new();
                                for pattern in patterns.iter().rev() {
                                    match pattern {
                                        FnPattern::Var(v) => {
                                            param_names.insert(0, v.clone());
                                        }
                                        FnPattern::Ctor(ctor_name, bindings) => {
                                            // Generate a temporary name for the argument
                                            let arg_name = format!("_arg{}", param_names.len());
                                            param_names.insert(0, arg_name.clone());
                                            
                                            // Wrap body: let binding_n = getfield n arg in ... in body
                                            for (idx, binding) in bindings.iter().enumerate().rev() {
                                                if binding != "_" {
                                                    // Create: let binding = (getfield idx arg) in body
                                                    let field_access = compile::Expr::App(
                                                        Box::new(compile::Expr::Var(format!("__getfield_{}_{}", arg_name, idx))),
                                                        vec![compile::Expr::Var(arg_name.clone())]
                                                    );
                                                    body_expr = compile::Expr::Let(
                                                        binding.clone(),
                                                        Box::new(compile::Expr::Var(format!("__field_{}_{}", arg_name, idx))),
                                                        Box::new(body_expr)
                                                    );
                                                }
                                            }
                                        }
                                        FnPattern::Literal(_) => {
                                            // Literal patterns need runtime matching
                                            param_names.insert(0, format!("_lit{}", param_names.len()));
                                        }
                                    }
                                }
                                
                                // Compile with pattern-aware compilation
                                let block = compile_with_patterns(&param_names, &patterns, &body_expr);
                                
                                output.push_str(&format!("fn {}({}) {{\n", fn_name, args));
                                for instr in &block.code {
                                    output.push_str(&format!("    {}\n", print_instr(instr, &ctx)));
                                }
                                output.push_str("}\n\n");
                            }
                            Err(_e) => continue,
                        },
                        Err(_e) => continue,
                    }
                }
                Err(_) => {
                    // Failed to parse patterns, skip with comment
                    output.push_str(&format!("-- {} pattern parse failed\n", name));
                    output.push_str(&format!("-- {} {} = {}\n\n", name, args, body));
                }
            }
        }
    }
    
    // Compile main
    if let Some(main) = main_expr {
        match Parser::new(&main) {
            Ok(mut parser) => match parser.parse_expr() {
                Ok(port_expr) => {
                    let expr = convert_expr(&port_expr);
                    let block = compile::Compiler::compile(&expr);
                    output.push_str("fn main() {\n");
                    for instr in &block.code {
                        output.push_str(&format!("    {}\n", print_instr(instr, &ctx)));
                    }
                    output.push_str("}\n");
                }
                Err(e) => {
                    return Err(JsValue::from_str(&format!("Parse error in main: {}", e)));
                }
            },
            Err(e) => {
                return Err(JsValue::from_str(&format!("Parse error in main: {}", e)));
            }
        }
    }
    
    Ok(output)
}

// Unit tests
#[cfg(test)]
mod tests {
    use super::compile;
    use super::*;
    
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
    
    #[test]
    fn test_function_call_via_closure() {
        use crate::port::parser::Parser;
        
        let mut st = Store::new();
        
        // Compile greet function body: x + 1 (with x as param)
        let body_src = "x + 1";
        let mut parser = Parser::new(body_src).unwrap();
        let port_expr = parser.parse_expr().unwrap();
        let expr = convert_expr(&port_expr);
        
        // Compile as function with parameter x
        let params = vec!["x".to_string()];
        let mut block = compile::Compiler::compile_function(&params, &expr);
        block.name = Some("greet".to_string());
        let code_hash = st.add_code(block);
        
        // Register as closure VALUE at name hash
        let name_hash = Hash::of_str("greet");
        let closure_val = value::Val::closure(code_hash, value::Env::new());
        st.set_value(name_hash, closure_val);
        
        // Compile main: greet 5
        let main_src = "greet 5";
        let mut parser = Parser::new(main_src).unwrap();
        let port_expr = parser.parse_expr().unwrap();
        let expr = convert_expr(&port_expr);
        let block = compile::Compiler::compile(&expr);
        let main_hash = st.add_code(block);
        
        // Run
        let mut vm = VM::new(&st);
        let result = vm.run(main_hash).expect("VM should run successfully");
        
        assert_eq!(result, Val::Int(6), "greet 5 should return 6");
    }
}
