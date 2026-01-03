//! Compiler from simple expressions to VM code

use crate::hash::Hash;
use crate::instr::{Instr, Literal, CodeBlock};
use crate::store::Store;

/// Simple AST for compilation
#[derive(Debug, Clone)]
pub enum Expr {
    Int(i64),
    Str(String),
    Bool(bool),
    Var(String),
    
    // Operations
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    UnOp(UnOp, Box<Expr>),
    
    // Control
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    
    // Functions
    Lambda(Vec<String>, Box<Expr>),
    App(Box<Expr>, Vec<Expr>),
    
    // Data
    List(Vec<Expr>),
    Tuple(Vec<Expr>),
    
    // Sequences
    Block(Vec<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
    Cons,
    Concat,  // String concatenation (++)
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg, Not,
    Head, Tail, IsNil,
}

/// Compilation context
pub struct Compiler {
    // Local variable -> slot mapping
    locals: Vec<(String, u32)>,
    // Generated code
    code: Vec<Instr>,
    // Lambda counter
    lambda_count: u32,
    // Collected lambdas
    lambdas: Vec<(String, Vec<String>, Expr)>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            locals: Vec::new(),
            code: Vec::new(),
            lambda_count: 0,
            lambdas: Vec::new(),
        }
    }

    /// Compile an expression to a code block
    pub fn compile(expr: &Expr) -> CodeBlock {
        let mut compiler = Compiler::new();
        compiler.compile_expr(expr);
        compiler.code.push(Instr::Halt);
        CodeBlock::new(compiler.code).with_name("main")
    }

    /// Compile a function body with parameters
    /// Parameters are assumed to be in the environment at indices 0, 1, 2, ...
    /// This differs from compile() in that variables are always loaded from env, not stack
    pub fn compile_function(params: &[String], body: &Expr) -> CodeBlock {
        let mut compiler = Compiler::new();
        // Register parameters as env slots (will use Load instruction)
        for (i, param) in params.iter().enumerate() {
            compiler.locals.push((param.clone(), i as u32));
        }
        // Compile body - compile_expr will use lookup_env_slot for variables
        compiler.compile_function_expr(body);
        compiler.code.push(Instr::Return);
        CodeBlock::new(compiler.code)
    }

    fn compile_function_expr(&mut self, expr: &Expr) {
        // Same as compile_expr but uses Load for all locals (function params)
        match expr {
            Expr::Int(n) => {
                self.emit(Instr::Push(Literal::Int(*n)));
            }
            Expr::Str(s) => {
                self.emit(Instr::Push(Literal::Str(s.clone())));
            }
            Expr::Bool(b) => {
                self.emit(Instr::Push(Literal::Bool(*b)));
            }
            Expr::Var(name) => {
                if let Some(slot) = self.lookup_env_slot(name) {
                    // Load from environment
                    self.emit(Instr::Load(slot));
                } else {
                    // Global lookup
                    self.emit(Instr::LoadGlobal(Hash::of_str(name)));
                }
            }
            Expr::BinOp(op, lhs, rhs) => {
                self.compile_function_expr(lhs);
                self.compile_function_expr(rhs);
                self.emit(match op {
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
                    BinOp::Cons => Instr::Cons,
                    BinOp::Concat => Instr::StrConcat,
                });
            }
            Expr::If(cond, then_e, else_e) => {
                self.compile_function_expr(cond);
                let jump_else_idx = self.code.len();
                self.emit(Instr::Nop);
                self.compile_function_expr(then_e);
                let jump_end_idx = self.code.len();
                self.emit(Instr::Nop);
                let else_start = self.code.len();
                self.compile_function_expr(else_e);
                let end_addr = self.code.len();
                let jump_to_else = (else_start as i32) - (jump_else_idx as i32);
                self.code[jump_else_idx] = Instr::JumpIfNot(jump_to_else);
                let jump_to_end = (end_addr as i32) - (jump_end_idx as i32);
                self.code[jump_end_idx] = Instr::Jump(jump_to_end);
            }
            Expr::App(func, args) => {
                for arg in args {
                    self.compile_function_expr(arg);
                }
                self.compile_function_expr(func);
                if args.len() == 1 {
                    self.emit(Instr::Apply);
                } else {
                    self.emit(Instr::ApplyN(args.len() as u8));
                }
            }
            Expr::List(elems) => {
                for elem in elems {
                    self.compile_function_expr(elem);
                }
                self.emit(Instr::MkList(elems.len() as u16));
            }
            _ => {
                // For other cases, fall back to regular compile
                self.compile_expr(expr);
            }
        }
    }

    /// Compile an expression and add to store
    pub fn compile_to_store(expr: &Expr, store: &mut Store) -> Hash {
        let block = Self::compile(expr);
        store.add_code(block)
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Int(n) => {
                self.emit(Instr::Push(Literal::Int(*n)));
            }
            Expr::Str(s) => {
                self.emit(Instr::Push(Literal::Str(s.clone())));
            }
            Expr::Bool(b) => {
                self.emit(Instr::Push(Literal::Bool(*b)));
            }
            Expr::Var(name) => {
                if let Some(depth) = self.lookup_depth(name) {
                    // Variable is on stack at depth `depth` from top
                    // We need to copy it to top: use Over for depth=1, or emit multiple ops
                    if depth == 0 {
                        self.emit(Instr::Dup);
                    } else if depth == 1 {
                        self.emit(Instr::Over);
                    } else {
                        // For deeper access, we'd need a Pick instruction
                        // For now, just use Load which requires env setup
                        self.emit(Instr::Load(depth as u32));
                    }
                } else {
                    // Try global lookup
                    self.emit(Instr::LoadGlobal(Hash::of_str(name)));
                }
            }

            Expr::BinOp(op, lhs, rhs) => {
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                self.emit(match op {
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
                    BinOp::Cons => Instr::Cons,
                    BinOp::Concat => Instr::StrConcat,
                });
            }

            Expr::UnOp(op, e) => {
                self.compile_expr(e);
                self.emit(match op {
                    UnOp::Neg => Instr::Neg,
                    UnOp::Not => Instr::Not,
                    UnOp::Head => Instr::Head,
                    UnOp::Tail => Instr::Tail,
                    UnOp::IsNil => Instr::IsNil,
                });
            }

            Expr::If(cond, then_e, else_e) => {
                // Compile condition
                self.compile_expr(cond);
                let jump_else_idx = self.code.len();
                self.emit(Instr::Nop); // placeholder for JumpIfNot
                
                // Compile then branch
                self.compile_expr(then_e);
                let jump_end_idx = self.code.len();
                self.emit(Instr::Nop); // placeholder for Jump
                
                // Compile else branch  
                let else_start = self.code.len();
                self.compile_expr(else_e);
                let end_addr = self.code.len();
                
                // Patch jumps
                // JumpIfNot goes to else_start
                let jump_to_else = (else_start as i32) - (jump_else_idx as i32);
                self.code[jump_else_idx] = Instr::JumpIfNot(jump_to_else);
                
                // Jump goes to end_addr  
                let jump_to_end = (end_addr as i32) - (jump_end_idx as i32);
                self.code[jump_end_idx] = Instr::Jump(jump_to_end);
            }

            Expr::Let(name, val, body) => {
                // Compile value - leaves it on stack
                self.compile_expr(val);
                // Track that this value is at current stack position
                let slot = self.locals.len() as u32;
                self.locals.push((name.clone(), slot));
                // DON'T emit Store - just leave value on stack and use Dup when needed
                // Actually, for simplicity, use environment-based approach
                // but we need to allocate env slots properly
                self.compile_expr(body);
                // Swap result with the let-bound value, then pop the let-bound value
                self.emit(Instr::Swap);
                self.emit(Instr::Pop);
                self.locals.pop();
            }

            Expr::Lambda(params, body) => {
                // Generate a unique name for this lambda
                let name = format!("lambda_{}", self.lambda_count);
                self.lambda_count += 1;
                
                // Collect for later compilation
                self.lambdas.push((name.clone(), params.clone(), (**body).clone()));
                
                // Emit closure creation
                self.emit(Instr::Closure(
                    Hash::of_str(&name),
                    self.locals.len() as u8
                ));
            }

            Expr::App(func, args) => {
                // Push arguments in order
                for arg in args {
                    self.compile_expr(arg);
                }
                // Compile function
                self.compile_expr(func);
                // Apply
                if args.len() == 1 {
                    self.emit(Instr::Apply);
                } else {
                    self.emit(Instr::ApplyN(args.len() as u8));
                }
            }

            Expr::List(elems) => {
                for elem in elems {
                    self.compile_expr(elem);
                }
                self.emit(Instr::MkList(elems.len() as u16));
            }

            Expr::Tuple(elems) => {
                for elem in elems {
                    self.compile_expr(elem);
                }
                self.emit(Instr::MkTuple(elems.len() as u8));
            }

            Expr::Block(exprs) => {
                for (i, e) in exprs.iter().enumerate() {
                    self.compile_expr(e);
                    // Pop intermediate values except last
                    if i < exprs.len() - 1 {
                        self.emit(Instr::Pop);
                    }
                }
            }
        }
    }

    fn emit(&mut self, instr: Instr) {
        self.code.push(instr);
    }

    #[allow(dead_code)]
    fn emit_placeholder(&mut self) -> usize {
        let addr = self.code.len();
        self.code.push(Instr::Nop);
        addr
    }

    #[allow(dead_code)]
    fn patch(&mut self, addr: usize, instr: Instr) {
        self.code[addr] = instr;
    }

    #[allow(dead_code)]
    fn current_addr(&self) -> usize {
        self.code.len()
    }

    fn lookup_depth(&self, name: &str) -> Option<usize> {
        // Find how deep the variable is from the top of the locals stack
        for (i, (n, _)) in self.locals.iter().rev().enumerate() {
            if n == name {
                return Some(i);
            }
        }
        None
    }

    fn lookup_env_slot(&self, name: &str) -> Option<u32> {
        // Find the slot number for a variable (used for function parameters)
        for (n, slot) in self.locals.iter() {
            if n == name {
                return Some(*slot);
            }
        }
        None
    }
}

// Expression builder DSL
impl Expr {
    pub fn int(n: i64) -> Self { Expr::Int(n) }
    pub fn str(s: impl Into<String>) -> Self { Expr::Str(s.into()) }
    pub fn bool(b: bool) -> Self { Expr::Bool(b) }
    pub fn var(s: impl Into<String>) -> Self { Expr::Var(s.into()) }

    pub fn add(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Add, Box::new(self), Box::new(other))
    }
    pub fn sub(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Sub, Box::new(self), Box::new(other))
    }
    pub fn mul(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Mul, Box::new(self), Box::new(other))
    }
    pub fn div(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Div, Box::new(self), Box::new(other))
    }
    pub fn eq(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Eq, Box::new(self), Box::new(other))
    }
    pub fn lt(self, other: Self) -> Self {
        Expr::BinOp(BinOp::Lt, Box::new(self), Box::new(other))
    }

    pub fn neg(self) -> Self {
        Expr::UnOp(UnOp::Neg, Box::new(self))
    }
    pub fn not(self) -> Self {
        Expr::UnOp(UnOp::Not, Box::new(self))
    }

    pub fn if_(cond: Self, then_: Self, else_: Self) -> Self {
        Expr::If(Box::new(cond), Box::new(then_), Box::new(else_))
    }

    pub fn let_(name: impl Into<String>, val: Self, body: Self) -> Self {
        Expr::Let(name.into(), Box::new(val), Box::new(body))
    }

    pub fn lambda(params: Vec<&str>, body: Self) -> Self {
        Expr::Lambda(
            params.into_iter().map(String::from).collect(),
            Box::new(body)
        )
    }

    pub fn app(func: Self, args: Vec<Self>) -> Self {
        Expr::App(Box::new(func), args)
    }

    pub fn list(elems: Vec<Self>) -> Self {
        Expr::List(elems)
    }

    pub fn tuple(elems: Vec<Self>) -> Self {
        Expr::Tuple(elems)
    }

    pub fn block(exprs: Vec<Self>) -> Self {
        Expr::Block(exprs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VM;
    use crate::store::Store;

    fn eval(expr: &Expr) -> crate::value::Val {
        let mut store = Store::new();
        let hash = Compiler::compile_to_store(expr, &mut store);
        VM::new(&store).run(hash).unwrap()
    }

    #[test]
    fn test_compile_int() {
        let result = eval(&Expr::int(42));
        assert_eq!(result.as_int(), Some(42));
    }

    #[test]
    fn test_compile_arithmetic() {
        // (2 + 3) * 4 = 20
        let expr = Expr::int(2).add(Expr::int(3)).mul(Expr::int(4));
        let result = eval(&expr);
        assert_eq!(result.as_int(), Some(20));
    }

    #[test]
    fn test_compile_if() {
        // if true then 1 else 2
        let expr = Expr::if_(
            Expr::bool(true),
            Expr::int(1),
            Expr::int(2)
        );
        let result = eval(&expr);
        assert_eq!(result.as_int(), Some(1));
    }

    #[test]
    fn test_compile_let() {
        // let x = 10 in x + 5
        let expr = Expr::let_(
            "x",
            Expr::int(10),
            Expr::var("x").add(Expr::int(5))
        );
        let result = eval(&expr);
        assert_eq!(result.as_int(), Some(15));
    }

    #[test]
    fn test_compile_list() {
        let expr = Expr::list(vec![
            Expr::int(1),
            Expr::int(2),
            Expr::int(3),
        ]);
        let result = eval(&expr);
        assert!(result.as_list().is_some());
        assert_eq!(result.as_list().unwrap().len(), 3);
    }
}
