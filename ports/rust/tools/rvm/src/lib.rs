//! RosettaVM Library - WebAssembly bindings
//!
//! This module provides the API for running Phi specs in the browser.

pub mod hash;
pub mod value;
pub mod instr;
pub mod store;
pub mod vm;
pub mod parse;
pub mod compile;
pub mod port;
pub mod phi_compiler;

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
