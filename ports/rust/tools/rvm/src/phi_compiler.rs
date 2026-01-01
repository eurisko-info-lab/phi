//! Phi to RVM Compiler
//! Compiles Phi source code to RVM bytecode
//!
//! This module provides a standalone Phi AST and compiler to RVM bytecode.

use std::collections::HashMap;
use crate::instr::{Instr, CodeBlock, Literal};
use crate::store::Store;
use crate::hash::Hash;

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnOp {
    Neg, Not,
}

/// Phi expression AST
#[derive(Debug, Clone)]
pub enum PhiExpr {
    // Literals
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    
    // Variable reference
    Var(String),
    
    // Lambda: \x -> body
    Lambda(String, Box<PhiExpr>),
    
    // Application: f x
    App(Box<PhiExpr>, Box<PhiExpr>),
    
    // Let binding: let x = e1 in e2
    Let(String, Box<PhiExpr>, Box<PhiExpr>),
    
    // Conditional: if cond then e1 else e2
    If(Box<PhiExpr>, Box<PhiExpr>, Box<PhiExpr>),
    
    // Binary operation
    BinOp(BinOp, Box<PhiExpr>, Box<PhiExpr>),
    
    // Unary operation
    UnOp(UnOp, Box<PhiExpr>),
    
    // List: [e1, e2, ...]
    List(Vec<PhiExpr>),
    
    // Tuple: (e1, e2, ...)
    Tuple(Vec<PhiExpr>),
    
    // Record: {f1 = e1, f2 = e2, ...}
    Record(Vec<(String, PhiExpr)>),
    
    // Match expression
    Match(Box<PhiExpr>, Vec<(Pattern, PhiExpr)>),
}

/// Pattern for match expressions
#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,                      // _
    Var(String),                   // x
    Int(i64),                      // 42
    Bool(bool),                    // true
    Str(String),                   // "hello"
    Constructor(String, Vec<Pattern>), // Cons(x, xs)
    List(Vec<Pattern>),            // [x, y, z]
    Tuple(Vec<Pattern>),           // (x, y)
}

/// Phi to RVM compiler
pub struct PhiCompiler {
    /// Local variable bindings (name -> env slot)
    locals: HashMap<String, u32>,
    /// Next available local slot
    next_local: u32,
    /// Generated code
    code: Vec<Instr>,
    /// Function table for forward references (reserved)
    #[allow(dead_code)]
    functions: HashMap<String, Hash>,
    /// Label counter for jumps (reserved)
    #[allow(dead_code)]
    label_counter: usize,
}

impl PhiCompiler {
    pub fn new() -> Self {
        PhiCompiler {
            locals: HashMap::new(),
            next_local: 0,
            code: Vec::new(),
            functions: HashMap::new(),
            label_counter: 0,
        }
    }

    /// Compile a Phi expression to RVM bytecode and store it
    pub fn compile_expr(&mut self, expr: &PhiExpr, store: &mut Store) -> Hash {
        self.code.clear();
        self.locals.clear();
        self.next_local = 0;
        
        self.emit_expr(expr);
        self.code.push(Instr::Halt);
        
        let block = CodeBlock::new(self.code.clone());
        store.add_code(block)
    }

    /// Compile to string representation (for printing)
    pub fn compile_to_string(&mut self, expr: &PhiExpr) -> Vec<String> {
        self.code.clear();
        self.locals.clear();
        self.next_local = 0;
        
        self.emit_expr(expr);
        
        self.code.iter().map(|i| format!("{:?}", i)).collect()
    }

    #[allow(dead_code)]
    fn fresh_label(&mut self) -> usize {
        self.label_counter += 1;
        self.label_counter
    }

    fn emit_expr(&mut self, expr: &PhiExpr) {
        match expr {
            PhiExpr::Int(n) => {
                self.code.push(Instr::Push(Literal::Int(*n)));
            }
            
            PhiExpr::Float(f) => {
                // RVM uses Float literal directly
                self.code.push(Instr::Push(Literal::Float(*f)));
            }
            
            PhiExpr::Bool(b) => {
                self.code.push(Instr::Push(Literal::Bool(*b)));
            }
            
            PhiExpr::Str(s) => {
                self.code.push(Instr::Push(Literal::Str(s.clone())));
            }
            
            PhiExpr::Var(name) => {
                if let Some(&slot) = self.locals.get(name) {
                    self.code.push(Instr::Load(slot));
                } else {
                    // Unknown variable - emit as string for now
                    self.code.push(Instr::Push(Literal::Str(name.clone())));
                }
            }
            
            PhiExpr::Lambda(param, body) => {
                // Save current locals
                let saved_locals = self.locals.clone();
                let saved_next = self.next_local;
                
                // Add parameter binding
                let slot = self.next_local;
                self.locals.insert(param.clone(), slot);
                self.next_local += 1;
                
                // Emit body
                self.emit_expr(body);
                
                // Restore locals
                self.locals = saved_locals;
                self.next_local = saved_next;
            }
            
            PhiExpr::App(func, arg) => {
                // Evaluate argument first, then function
                self.emit_expr(arg);
                self.emit_expr(func);
                // Apply closure
                self.code.push(Instr::Apply);
            }
            
            PhiExpr::Let(name, value, body) => {
                // Evaluate value
                self.emit_expr(value);
                
                // Store in local
                let slot = self.next_local;
                self.locals.insert(name.clone(), slot);
                self.next_local += 1;
                
                self.code.push(Instr::Store(slot));
                
                // Evaluate body
                self.emit_expr(body);
            }
            
            PhiExpr::If(cond, then_branch, else_branch) => {
                // Evaluate condition
                self.emit_expr(cond);
                
                // Record position for jump to else
                let else_jump_idx = self.code.len();
                self.code.push(Instr::Nop); // Placeholder for JumpIfNot
                
                // Then branch
                self.emit_expr(then_branch);
                
                // Jump over else
                let end_jump_idx = self.code.len();
                self.code.push(Instr::Nop); // Placeholder for Jump
                
                // Else branch starts here
                let else_start = self.code.len();
                self.emit_expr(else_branch);
                
                // End
                let end = self.code.len();
                
                // Patch jumps (relative offsets)
                self.code[else_jump_idx] = Instr::JumpIfNot((else_start - else_jump_idx - 1) as i32);
                self.code[end_jump_idx] = Instr::Jump((end - end_jump_idx - 1) as i32);
            }
            
            PhiExpr::BinOp(op, left, right) => {
                self.emit_expr(left);
                self.emit_expr(right);
                
                let instr = match op {
                    BinOp::Add => Instr::Add,
                    BinOp::Sub => Instr::Sub,
                    BinOp::Mul => Instr::Mul,
                    BinOp::Div => Instr::Div,
                    BinOp::Mod => Instr::Mod,
                    BinOp::Eq => Instr::Eq,
                    BinOp::Ne => Instr::Ne,
                    BinOp::Lt => Instr::Lt,
                    BinOp::Le => Instr::Le,
                    BinOp::Gt => Instr::Gt,
                    BinOp::Ge => Instr::Ge,
                    BinOp::And => Instr::And,
                    BinOp::Or => Instr::Or,
                };
                self.code.push(instr);
            }
            
            PhiExpr::UnOp(op, operand) => {
                self.emit_expr(operand);
                
                match op {
                    UnOp::Neg => self.code.push(Instr::Neg),
                    UnOp::Not => self.code.push(Instr::Not),
                }
            }
            
            PhiExpr::List(elems) => {
                // Build list using Cons
                self.code.push(Instr::Push(Literal::Nil)); // Start with empty list
                
                for elem in elems.iter().rev() {
                    self.emit_expr(elem);
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::Cons);
                }
            }
            
            PhiExpr::Tuple(elems) => {
                // Push all elements and construct tuple
                for elem in elems {
                    self.emit_expr(elem);
                }
                self.code.push(Instr::MkTuple(elems.len() as u8));
            }
            
            PhiExpr::Record(fields) => {
                // For now, treat as tuple with field names stored separately
                for (_, value) in fields {
                    self.emit_expr(value);
                }
                self.code.push(Instr::MkRecord(fields.len() as u8));
            }
            
            PhiExpr::Match(scrutinee, arms) => {
                self.emit_expr(scrutinee);
                
                // Simple match: try each pattern in order
                let mut end_jumps = Vec::new();
                
                for (pattern, body) in arms.iter() {
                    // Duplicate scrutinee for pattern test
                    self.code.push(Instr::Dup);
                    
                    // Test pattern - stores result on stack
                    let _fail_jump_idx = self.code.len();
                    self.emit_pattern_test(pattern);
                    
                    // If pattern failed, record jump position
                    let next_arm_idx = self.code.len();
                    self.code.push(Instr::Nop); // Placeholder
                    
                    // Pop scrutinee copy (pattern matched)
                    self.code.push(Instr::Pop);
                    
                    // Emit body
                    self.emit_expr(body);
                    
                    // Jump to end
                    end_jumps.push(self.code.len());
                    self.code.push(Instr::Nop);
                    
                    // Next arm starts here - patch jump
                    let next_arm = self.code.len();
                    self.code[next_arm_idx] = Instr::JumpIfNot((next_arm - next_arm_idx - 1) as i32);
                }
                
                // Match failure - should be unreachable with exhaustive patterns
                self.code.push(Instr::Push(Literal::Str("match failure".to_string())));
                self.code.push(Instr::Print);
                self.code.push(Instr::Halt);
                
                // Patch end jumps
                let end = self.code.len();
                for idx in end_jumps {
                    self.code[idx] = Instr::Jump((end - idx - 1) as i32);
                }
            }
        }
    }

    fn emit_pattern_test(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard => {
                // Always matches
                self.code.push(Instr::Pop);
                self.code.push(Instr::Push(Literal::Bool(true)));
            }
            
            Pattern::Var(name) => {
                // Bind to local
                let slot = self.next_local;
                self.locals.insert(name.clone(), slot);
                self.next_local += 1;
                self.code.push(Instr::Store(slot));
                self.code.push(Instr::Push(Literal::Bool(true)));
            }
            
            Pattern::Int(n) => {
                self.code.push(Instr::Push(Literal::Int(*n)));
                self.code.push(Instr::Eq);
            }
            
            Pattern::Bool(b) => {
                self.code.push(Instr::Push(Literal::Bool(*b)));
                self.code.push(Instr::Eq);
            }
            
            Pattern::Str(s) => {
                self.code.push(Instr::Push(Literal::Str(s.clone())));
                self.code.push(Instr::Eq);
            }
            
            Pattern::Constructor(name, sub_patterns) => {
                // Test constructor tag
                let tag = simple_hash(name);
                self.code.push(Instr::TestTag(tag));
                // If matched, recursively test sub-patterns
                if !sub_patterns.is_empty() {
                    for (i, pat) in sub_patterns.iter().enumerate() {
                        self.code.push(Instr::Dup);
                        self.code.push(Instr::GetField(i as u8));
                        self.emit_pattern_test(pat);
                        // AND results
                        self.code.push(Instr::And);
                    }
                }
            }
            
            Pattern::List(pats) => {
                // Test list length and elements
                self.code.push(Instr::Push(Literal::Bool(true))); // Accumulator
                for pat in pats {
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::Dup);
                    self.code.push(Instr::IsNil);
                    self.code.push(Instr::Not);
                    // Get head
                    self.code.push(Instr::Dup);
                    self.code.push(Instr::Head);
                    self.emit_pattern_test(pat);
                    self.code.push(Instr::And);
                    // Move to tail
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::Tail);
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::And);
                }
                // Final check: list should be nil
                self.code.push(Instr::Swap);
                self.code.push(Instr::IsNil);
                self.code.push(Instr::And);
            }
            
            Pattern::Tuple(pats) => {
                self.code.push(Instr::Push(Literal::Bool(true)));
                for (i, pat) in pats.iter().enumerate() {
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::Dup);
                    self.code.push(Instr::GetField(i as u8));
                    self.emit_pattern_test(pat);
                    self.code.push(Instr::Swap);
                    self.code.push(Instr::Pop);
                    self.code.push(Instr::And);
                }
                self.code.push(Instr::Swap);
                self.code.push(Instr::Pop);
            }
        }
    }
}

/// Simple string hash for constructor tags
fn simple_hash(name: &str) -> u8 {
    (name.bytes().fold(0u32, |acc, b| acc.wrapping_mul(31).wrapping_add(b as u32)) & 0xFF) as u8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compile_int() {
        let mut compiler = PhiCompiler::new();
        let instrs = compiler.compile_to_string(&PhiExpr::Int(42));
        assert!(instrs.iter().any(|i| i.contains("42")));
    }

    #[test]
    fn test_compile_binop() {
        let mut compiler = PhiCompiler::new();
        let expr = PhiExpr::BinOp(
            BinOp::Add,
            Box::new(PhiExpr::Int(1)),
            Box::new(PhiExpr::Int(2)),
        );
        let instrs = compiler.compile_to_string(&expr);
        assert!(instrs.iter().any(|i| i.contains("Add")));
    }
    
    #[test]
    fn test_compile_if() {
        let mut compiler = PhiCompiler::new();
        let expr = PhiExpr::If(
            Box::new(PhiExpr::Bool(true)),
            Box::new(PhiExpr::Int(1)),
            Box::new(PhiExpr::Int(2)),
        );
        let instrs = compiler.compile_to_string(&expr);
        assert!(instrs.iter().any(|i| i.contains("JumpIfNot")));
    }
}
