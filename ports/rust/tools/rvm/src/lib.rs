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
    
    // Check if this looks like a full program (has main =)
    let is_program = source.lines()
        .any(|line| {
            let trimmed = line.trim();
            trimmed.starts_with("main") && trimmed.contains("=")
        });
    
    let main_expr = if is_program {
        // Extract main expression from program
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
            if line.starts_with("main") && line.contains("=") {
                if let Some(pos) = line.find('=') {
                    main_body = Some(line[pos + 1..].trim().to_string());
                }
            }
        }
        main_body.ok_or_else(|| JsValue::from_str("No main expression found"))?
    } else {
        source.to_string()
    };
    
    // Parse and compile the main expression
    let mut parser = Parser::new(&main_expr)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    let port_expr = parser.parse_expr()
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    
    let expr = convert_expr(&port_expr);
    let block = compile::Compiler::compile(&expr);
    
    let mut st = store::Store::new();
    let hash = st.add_code(block);
    let mut machine = vm::VM::new(&st);
    
    let result = machine.run(hash)
        .map_err(|e| JsValue::from_str(&format!("Runtime error: {}", e)))?;
    
    Ok(format!("{:?}", result))
}

// Helper function to format an instruction as RVM assembly text
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn format_instr(instr: &crate::instr::Instr) -> String {
    use crate::instr::{Instr, Literal};
    match instr {
        // Stack
        Instr::Push(lit) => match lit {
            Literal::Int(n) => format!("PUSH {}", n),
            Literal::Float(f) => format!("PUSH {}", f),
            Literal::Str(s) => format!("PUSH \"{}\"", s),
            Literal::Bool(b) => format!("PUSH {}", b),
            Literal::Nil => "PUSH nil".to_string(),
            Literal::Unit => "PUSH unit".to_string(),
            Literal::Hash(h) => format!("PUSH @{:?}", h),
        },
        Instr::Pop => "POP".to_string(),
        Instr::Dup => "DUP".to_string(),
        Instr::Swap => "SWAP".to_string(),
        Instr::Rot => "ROT".to_string(),
        Instr::Over => "OVER".to_string(),
        
        // Env
        Instr::Load(n) => format!("LOAD {}", n),
        Instr::Store(n) => format!("STORE {}", n),
        Instr::LoadGlobal(s) => format!("LOADG {}", s),
        Instr::StoreGlobal(s) => format!("STOREG {}", s),
        
        // Arithmetic
        Instr::Add => "ADD".to_string(),
        Instr::Sub => "SUB".to_string(),
        Instr::Mul => "MUL".to_string(),
        Instr::Div => "DIV".to_string(),
        Instr::Mod => "MOD".to_string(),
        Instr::Neg => "NEG".to_string(),
        
        // Comparison
        Instr::Eq => "EQ".to_string(),
        Instr::Ne => "NE".to_string(),
        Instr::Lt => "LT".to_string(),
        Instr::Le => "LE".to_string(),
        Instr::Gt => "GT".to_string(),
        Instr::Ge => "GE".to_string(),
        
        // Boolean
        Instr::Not => "NOT".to_string(),
        Instr::And => "AND".to_string(),
        Instr::Or => "OR".to_string(),
        
        // Control
        Instr::Jump(n) => format!("JMP {}", n),
        Instr::JumpIf(n) => format!("JT {}", n),
        Instr::JumpIfNot(n) => format!("JF {}", n),
        Instr::Call(n) => format!("CALL {}", n),
        Instr::TailCall(n) => format!("TAILCALL {}", n),
        Instr::Return => "RET".to_string(),
        Instr::Halt => "HALT".to_string(),
        
        // Data structures
        Instr::MkList(n) => format!("MKLIST {}", n),
        Instr::MkTuple(n) => format!("MKTUPLE {}", n),
        Instr::Closure(h, n) => format!("CLOSURE @{:?} {}", h, n),
        Instr::Apply => "APPLY".to_string(),
        Instr::ApplyN(n) => format!("APPLYN {}", n),
        Instr::Index => "INDEX".to_string(),
        Instr::GetField(n) => format!("GETFIELD {}", n),
        
        // Lists
        Instr::Cons => "CONS".to_string(),
        Instr::Head => "HEAD".to_string(),
        Instr::Tail => "TAIL".to_string(),
        Instr::IsNil => "ISNIL".to_string(),
        Instr::Len => "LEN".to_string(),
        
        // Fallback for any other instructions
        _ => format!("{:?}", instr),
    }
}

// Helper function to convert port::expr::Expr to compile::Expr
#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn convert_expr(e: &crate::port::expr::Expr) -> compile::Expr {
    use crate::port::expr::Expr as PortExpr;
    match e {
        PortExpr::Int(n) => compile::Expr::Int(*n),
        PortExpr::String(s) => compile::Expr::Str((**s).clone()),
        PortExpr::Bool(b) => compile::Expr::Bool(*b),
        PortExpr::Unit => compile::Expr::Tuple(vec![]),
        PortExpr::Var(name) => compile::Expr::Var(name.clone()),
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
    
    let mut parser = Parser::new(source)
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    let port_expr = parser.parse_expr()
        .map_err(|e| JsValue::from_str(&format!("Parse error: {}", e)))?;
    
    // Convert to compile::Expr and compile
    let expr = convert_expr(&port_expr);
    let block = compile::Compiler::compile(&expr);
    
    // Format as RVM assembly
    let mut output = String::new();
    output.push_str("fn main() {\n");
    for instr in &block.code {
        output.push_str(&format!("    {}\n", format_instr(instr)));
    }
    output.push_str("}\n");
    
    Ok(output)
}

#[cfg(all(feature = "wasm", target_arch = "wasm32"))]
fn compile_phi_program(source: &str) -> Result<String, JsValue> {
    use std::collections::HashMap;
    use crate::port::parser::Parser;
    
    let mut output = String::new();
    let mut function_cases: HashMap<String, Vec<(String, String)>> = HashMap::new();
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
        for (i, (args, body)) in cases.iter().enumerate() {
            // Skip functions with pattern matching (args contain non-identifiers)
            let has_patterns = args.split_whitespace().any(|arg| {
                !arg.chars().all(|c| c.is_alphanumeric() || c == '_') ||
                arg.chars().next().map(|c| c.is_numeric()).unwrap_or(false) ||
                arg.starts_with('(') ||
                arg == "_"
            });
            if has_patterns && !args.is_empty() {
                // Skip pattern matching functions for now
                continue;
            }
            
            let fn_name = if cases.len() > 1 {
                format!("{}_{}", name, i)
            } else {
                name.clone()
            };
            
            // Parse and compile body
            let full_expr = if args.is_empty() {
                body.clone()
            } else {
                format!("\\{} -> {}", args, body)
            };
            
            match Parser::new(&full_expr) {
                Ok(mut parser) => match parser.parse_expr() {
                    Ok(port_expr) => {
                        let expr = convert_expr(&port_expr);
                        let block = compile::Compiler::compile(&expr);
                        output.push_str(&format!("fn {}() {{\n", fn_name));
                        for instr in &block.code {
                            output.push_str(&format!("    {}\n", format_instr(instr)));
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
                        output.push_str(&format!("    {}\n", format_instr(instr)));
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
