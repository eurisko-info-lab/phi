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
            // Check for pattern matching in args (args contain non-identifiers)
            let has_patterns = args.split_whitespace().any(|arg| {
                !arg.chars().all(|c| c.is_alphanumeric() || c == '_') ||
                arg.chars().next().map(|c| c.is_numeric()).unwrap_or(false) ||
                arg.starts_with('(') ||
                arg == "_"
            });
            
            if has_patterns && !args.is_empty() {
                // Generate a stub that reports the pattern matching isn't implemented
                output.push_str(&format!("-- {} has pattern matching (not yet compiled)\n", name));
                output.push_str(&format!("-- {} {} = {}\n\n", name, args, body));
                continue;
            }
            
            let fn_name = if cases.len() > 1 {
                format!("{}_{}", name, i)
            } else {
                name.clone()
            };
            
            // Parse and compile body directly (not as lambda)
            match Parser::new(body) {
                Ok(mut parser) => match parser.parse_expr() {
                    Ok(port_expr) => {
                        let expr = convert_expr(&port_expr);
                        let params: Vec<String> = args.split_whitespace()
                            .map(|s| s.to_string())
                            .collect();
                        let block = if params.is_empty() {
                            compile::Compiler::compile(&expr)
                        } else {
                            compile::Compiler::compile_function(&params, &expr)
                        };
                        output.push_str(&format!("fn {}({}) {{\n", fn_name, args));
                        for instr in &block.code {
                            output.push_str(&format!("    {}\n", print_instr(instr, &ctx)));
                        }
                        output.push_str("}\n\n");
                    }
                    Err(_e) => {
                        // Skip functions that fail to parse (likely pattern matching)
                        continue;
                    }
                },
                Err(_e) => {
                    // Skip functions that fail to parse
                    continue;
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
