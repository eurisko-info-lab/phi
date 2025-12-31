//! RosettaVM execution engine

use std::rc::Rc;
use crate::hash::Hash;
use crate::instr::{Instr, Literal, BuiltinOp};
#[cfg(test)]
use crate::instr::CodeBlock;
use crate::value::{Val, Env, Frame};
use crate::store::Store;

/// VM execution state
pub struct VM<'a> {
    // Store reference
    store: &'a Store,
    // Value stack
    stack: Vec<Val>,
    // Call stack
    frames: Vec<Frame>,
    // Current environment
    env: Env,
    // Current code block
    code: Option<Hash>,
    // Program counter
    pc: usize,
    // Halted?
    halted: bool,
    // Step count (for limits)
    steps: u64,
    // Debug mode
    debug: bool,
}

/// Execution result
pub type VMResult<T> = Result<T, VMError>;

/// VM errors
#[derive(Debug, Clone)]
pub enum VMError {
    StackUnderflow,
    TypeMismatch { expected: &'static str, got: String },
    UndefinedHash(Hash),
    UndefinedName(String),
    InvalidPC(usize),
    DivisionByZero,
    IndexOutOfBounds(usize, usize),
    InvalidArity { expected: u8, got: usize },
    NoCode,
    StepLimitExceeded(u64),
    AssertionFailed(String),
    NotImplemented(String),
}

impl std::fmt::Display for VMError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMError::StackUnderflow => write!(f, "stack underflow"),
            VMError::TypeMismatch { expected, got } => 
                write!(f, "type mismatch: expected {}, got {}", expected, got),
            VMError::UndefinedHash(h) => write!(f, "undefined hash: {}", h.short()),
            VMError::UndefinedName(n) => write!(f, "undefined name: {}", n),
            VMError::InvalidPC(pc) => write!(f, "invalid PC: {}", pc),
            VMError::DivisionByZero => write!(f, "division by zero"),
            VMError::IndexOutOfBounds(i, len) => 
                write!(f, "index {} out of bounds (len {})", i, len),
            VMError::InvalidArity { expected, got } =>
                write!(f, "invalid arity: expected {}, got {}", expected, got),
            VMError::NoCode => write!(f, "no code to execute"),
            VMError::StepLimitExceeded(n) => write!(f, "step limit exceeded: {}", n),
            VMError::AssertionFailed(msg) => write!(f, "assertion failed: {}", msg),
            VMError::NotImplemented(op) => write!(f, "not implemented: {}", op),
        }
    }
}

impl std::error::Error for VMError {}

impl<'a> VM<'a> {
    pub fn new(store: &'a Store) -> Self {
        VM {
            store,
            stack: Vec::with_capacity(1024),
            frames: Vec::with_capacity(256),
            env: Env::new(),
            code: None,
            pc: 0,
            halted: false,
            steps: 0,
            debug: false,
        }
    }

    pub fn debug(mut self, on: bool) -> Self {
        self.debug = on;
        self
    }

    /// Load and run a function by hash
    pub fn run(&mut self, hash: Hash) -> VMResult<Val> {
        self.code = Some(hash);
        self.pc = 0;
        self.halted = false;
        
        while !self.halted {
            self.step()?;
        }
        
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    /// Load and run a function by name
    pub fn run_named(&mut self, name: &str) -> VMResult<Val> {
        let hash = self.store.resolve(name)
            .ok_or_else(|| VMError::UndefinedName(name.to_string()))?;
        self.run(hash)
    }

    /// Push arguments for a call
    pub fn push_args(&mut self, args: Vec<Val>) {
        self.stack.extend(args);
    }

    /// Execute one instruction
    pub fn step(&mut self) -> VMResult<()> {
        self.steps += 1;
        if self.steps > 10_000_000 {
            return Err(VMError::StepLimitExceeded(self.steps));
        }

        let code_hash = self.code.ok_or(VMError::NoCode)?;
        let block = self.store.get_code(&code_hash)
            .ok_or(VMError::UndefinedHash(code_hash))?;
        
        let instr = block.get(self.pc)
            .ok_or(VMError::InvalidPC(self.pc))?
            .clone();

        if self.debug {
            eprintln!("[{:04}] {:?}  stack={}", self.pc, instr, self.stack.len());
        }

        self.pc += 1;
        self.exec(instr)
    }

    fn exec(&mut self, instr: Instr) -> VMResult<()> {
        match instr {
            // Stack ops
            Instr::Push(lit) => {
                self.stack.push(self.literal_to_val(lit));
            }
            Instr::Pop => {
                self.stack.pop().ok_or(VMError::StackUnderflow)?;
            }
            Instr::Dup => {
                let v = self.peek(0)?.clone();
                self.stack.push(v);
            }
            Instr::Swap => {
                let len = self.stack.len();
                if len < 2 { return Err(VMError::StackUnderflow); }
                self.stack.swap(len - 1, len - 2);
            }
            Instr::Rot => {
                let len = self.stack.len();
                if len < 3 { return Err(VMError::StackUnderflow); }
                // a b c -> b c a
                let c = self.stack.pop().unwrap();
                let b = self.stack.pop().unwrap();
                let a = self.stack.pop().unwrap();
                self.stack.push(b);
                self.stack.push(c);
                self.stack.push(a);
            }
            Instr::Over => {
                let v = self.peek(1)?.clone();
                self.stack.push(v);
            }

            // Environment
            Instr::Load(idx) => {
                let v = self.env.get(idx as usize)
                    .ok_or(VMError::IndexOutOfBounds(idx as usize, 0))?
                    .clone();
                self.stack.push(v);
            }
            Instr::Store(idx) => {
                let v = self.pop()?;
                self.env.set(idx as usize, v);
            }
            Instr::LoadGlobal(hash) => {
                let v = self.store.get_value(&hash)
                    .ok_or(VMError::UndefinedHash(hash))?
                    .clone();
                self.stack.push(v);
            }
            Instr::StoreGlobal(_) => {
                return Err(VMError::NotImplemented("StoreGlobal".into()));
            }

            // Arithmetic
            Instr::Add => self.binop_int(|a, b| a + b)?,
            Instr::Sub => self.binop_int(|a, b| a - b)?,
            Instr::Mul => self.binop_int(|a, b| a * b)?,
            Instr::Div => {
                let b = self.pop_int()?;
                let a = self.pop_int()?;
                if b == 0 { return Err(VMError::DivisionByZero); }
                self.stack.push(Val::Int(a / b));
            }
            Instr::Mod => {
                let b = self.pop_int()?;
                let a = self.pop_int()?;
                if b == 0 { return Err(VMError::DivisionByZero); }
                self.stack.push(Val::Int(a % b));
            }
            Instr::Neg => {
                let a = self.pop_int()?;
                self.stack.push(Val::Int(-a));
            }

            // Comparison
            Instr::Eq => self.cmp_op(|a, b| a == b)?,
            Instr::Ne => self.cmp_op(|a, b| a != b)?,
            Instr::Lt => self.cmp_int(|a, b| a < b)?,
            Instr::Le => self.cmp_int(|a, b| a <= b)?,
            Instr::Gt => self.cmp_int(|a, b| a > b)?,
            Instr::Ge => self.cmp_int(|a, b| a >= b)?,

            // Boolean
            Instr::Not => {
                let b = self.pop_bool()?;
                self.stack.push(Val::Bool(!b));
            }
            Instr::And => self.binop_bool(|a, b| a && b)?,
            Instr::Or => self.binop_bool(|a, b| a || b)?,

            // Control flow
            Instr::Jump(offset) => {
                self.pc = (self.pc as i32 + offset - 1) as usize;
            }
            Instr::JumpIf(offset) => {
                if self.pop_bool()? {
                    self.pc = (self.pc as i32 + offset - 1) as usize;
                }
            }
            Instr::JumpIfNot(offset) => {
                if !self.pop_bool()? {
                    self.pc = (self.pc as i32 + offset - 1) as usize;
                }
            }
            Instr::Call(hash) => {
                self.call(hash)?;
            }
            Instr::TailCall(hash) => {
                self.code = Some(hash);
                self.pc = 0;
                // Don't push frame - reuse current
            }
            Instr::CallN(hash, n) => {
                // Collect N args
                let mut args = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    args.push(self.pop()?);
                }
                args.reverse();
                self.env = self.env.extend(args);
                self.call(hash)?;
            }
            Instr::Return => {
                if let Some(frame) = self.frames.pop() {
                    self.code = Some(frame.return_code);
                    self.pc = frame.return_pc;
                    self.env = frame.saved_env;
                } else {
                    self.halted = true;
                }
            }
            Instr::Halt => {
                self.halted = true;
            }

            // Closures
            Instr::Closure(code_hash, num_captures) => {
                let mut captures = Vec::with_capacity(num_captures as usize);
                for i in 0..num_captures {
                    if let Some(v) = self.env.get(i as usize) {
                        captures.push(v.clone());
                    }
                }
                let closure_env = Env::new().extend(captures);
                self.stack.push(Val::closure(code_hash, closure_env));
            }
            Instr::Apply => {
                let closure = self.pop()?;
                let arg = self.pop()?;
                if let Some((code_hash, env)) = closure.as_closure() {
                    let mut new_env = env.clone();
                    new_env.push(arg);
                    self.push_frame();
                    self.code = Some(code_hash);
                    self.pc = 0;
                    self.env = new_env;
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "closure", 
                        got: format!("{}", closure) 
                    });
                }
            }
            Instr::ApplyN(n) => {
                let closure = self.pop()?;
                let mut args = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    args.push(self.pop()?);
                }
                args.reverse();
                
                if let Some((code_hash, env)) = closure.as_closure() {
                    let new_env = env.extend(args);
                    self.push_frame();
                    self.code = Some(code_hash);
                    self.pc = 0;
                    self.env = new_env;
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "closure", 
                        got: format!("{}", closure) 
                    });
                }
            }
            Instr::PartialApply(_n) => {
                return Err(VMError::NotImplemented("PartialApply".into()));
            }

            // Data
            Instr::MkTuple(n) => {
                let mut elems = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    elems.push(self.pop()?);
                }
                elems.reverse();
                self.stack.push(Val::tuple(elems));
            }
            Instr::MkList(n) => {
                let mut elems = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    elems.push(self.pop()?);
                }
                elems.reverse();
                self.stack.push(Val::list(elems));
            }
            Instr::MkRecord(_n) => {
                return Err(VMError::NotImplemented("MkRecord".into()));
            }
            Instr::MkCon(type_hash, tag, n) => {
                let mut fields = Vec::with_capacity(n as usize);
                for _ in 0..n {
                    fields.push(self.pop()?);
                }
                fields.reverse();
                self.stack.push(Val::con(type_hash, tag as u32, fields));
            }
            Instr::GetField(idx) => {
                let v = self.pop()?;
                match v {
                    Val::Tuple(t) => {
                        let val = t.get(idx as usize)
                            .ok_or(VMError::IndexOutOfBounds(idx as usize, t.len()))?
                            .clone();
                        self.stack.push(val);
                    }
                    Val::Con(_, _, fields) => {
                        let val = fields.get(idx as usize)
                            .ok_or(VMError::IndexOutOfBounds(idx as usize, fields.len()))?
                            .clone();
                        self.stack.push(val);
                    }
                    _ => return Err(VMError::TypeMismatch { 
                        expected: "tuple or constructor", 
                        got: format!("{}", v) 
                    }),
                }
            }
            Instr::SetField(_) => {
                return Err(VMError::NotImplemented("SetField".into()));
            }

            // Pattern matching
            Instr::Match(arms) => {
                let v = self.peek(0)?;
                if let Some((_, tag, _)) = v.as_con() {
                    for arm in arms {
                        if arm.tag == 255 || arm.tag as u32 == tag {
                            self.pc = (self.pc as i32 + arm.offset - 1) as usize;
                            break;
                        }
                    }
                } else {
                    // Default case
                    for arm in arms {
                        if arm.tag == 255 {
                            self.pc = (self.pc as i32 + arm.offset - 1) as usize;
                            break;
                        }
                    }
                }
            }
            Instr::TestTag(tag) => {
                let v = self.peek(0)?;
                let matches = v.as_con()
                    .map(|(_, t, _)| t == tag as u32)
                    .unwrap_or(false);
                self.stack.push(Val::Bool(matches));
            }
            Instr::Unpack(n) => {
                let v = self.pop()?;
                if let Some((_, _, fields)) = v.as_con() {
                    for i in 0..n.min(fields.len() as u8) {
                        self.stack.push(fields[i as usize].clone());
                    }
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "constructor", 
                        got: format!("{}", v) 
                    });
                }
            }

            // Lists
            Instr::Cons => {
                let tail = self.pop()?;
                let head = self.pop()?;
                match tail {
                    Val::List(l) => {
                        let mut new_list = vec![head];
                        new_list.extend(Rc::try_unwrap(l).unwrap_or_else(|rc| (*rc).clone()));
                        self.stack.push(Val::list(new_list));
                    }
                    Val::Nil => {
                        self.stack.push(Val::list(vec![head]));
                    }
                    _ => return Err(VMError::TypeMismatch { 
                        expected: "list", 
                        got: format!("{}", tail) 
                    }),
                }
            }
            Instr::Head => {
                let v = self.pop()?;
                if let Some(l) = v.as_list() {
                    let h = l.first().cloned().unwrap_or(Val::Nil);
                    self.stack.push(h);
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "list", 
                        got: format!("{}", v) 
                    });
                }
            }
            Instr::Tail => {
                let v = self.pop()?;
                if let Some(l) = v.as_list() {
                    let t = if l.len() > 1 {
                        Val::list(l[1..].to_vec())
                    } else {
                        Val::Nil
                    };
                    self.stack.push(t);
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "list", 
                        got: format!("{}", v) 
                    });
                }
            }
            Instr::IsNil => {
                let v = self.pop()?;
                let is_nil = matches!(v, Val::Nil) || 
                    v.as_list().map(|l| l.is_empty()).unwrap_or(false);
                self.stack.push(Val::Bool(is_nil));
            }
            Instr::Len => {
                let v = self.pop()?;
                let len = match &v {
                    Val::List(l) => l.len() as i64,
                    Val::Str(s) => s.len() as i64,
                    Val::Nil => 0,
                    _ => return Err(VMError::TypeMismatch { 
                        expected: "list or string", 
                        got: format!("{}", v) 
                    }),
                };
                self.stack.push(Val::Int(len));
            }
            Instr::Concat => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Val::List(la), Val::List(lb)) => {
                        let mut new_list = Rc::try_unwrap(la).unwrap_or_else(|rc| (*rc).clone());
                        new_list.extend((*lb).clone());
                        self.stack.push(Val::list(new_list));
                    }
                    _ => return Err(VMError::TypeMismatch { 
                        expected: "lists", 
                        got: "non-list".into() 
                    }),
                }
            }
            Instr::Index => {
                let idx = self.pop_int()? as usize;
                let v = self.pop()?;
                if let Some(l) = v.as_list() {
                    let elem = l.get(idx)
                        .ok_or(VMError::IndexOutOfBounds(idx, l.len()))?
                        .clone();
                    self.stack.push(elem);
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "list", 
                        got: format!("{}", v) 
                    });
                }
            }

            // Strings
            Instr::StrConcat => {
                let b = self.pop()?;
                let a = self.pop()?;
                match (a, b) {
                    (Val::Str(sa), Val::Str(sb)) => {
                        self.stack.push(Val::str(format!("{}{}", sa, sb)));
                    }
                    _ => return Err(VMError::TypeMismatch { 
                        expected: "strings", 
                        got: "non-string".into() 
                    }),
                }
            }
            Instr::StrLen => {
                let v = self.pop()?;
                if let Some(s) = v.as_str() {
                    self.stack.push(Val::Int(s.len() as i64));
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "string", 
                        got: format!("{}", v) 
                    });
                }
            }
            Instr::StrSlice => {
                let end = self.pop_int()? as usize;
                let start = self.pop_int()? as usize;
                let v = self.pop()?;
                if let Some(s) = v.as_str() {
                    let slice = &s[start.min(s.len())..end.min(s.len())];
                    self.stack.push(Val::str(slice));
                } else {
                    return Err(VMError::TypeMismatch { 
                        expected: "string", 
                        got: format!("{}", v) 
                    });
                }
            }

            // Effects (stub)
            Instr::Perform(_) | Instr::Handle(_) | Instr::Resume => {
                return Err(VMError::NotImplemented("effects".into()));
            }

            // Builtins
            Instr::Builtin(op) => self.builtin(op)?,

            // Debug
            Instr::Print => {
                let v = self.peek(0)?;
                println!("{}", v);
            }
            Instr::TypeOf => {
                let v = self.pop()?;
                let ty = match v {
                    Val::Int(_) => "Int",
                    Val::Str(_) => "String",
                    Val::Bool(_) => "Bool",
                    Val::Float(_) => "Float",
                    Val::List(_) => "List",
                    Val::Tuple(_) => "Tuple",
                    Val::Record(_) => "Record",
                    Val::Con(_, _, _) => "Con",
                    Val::Closure(_, _) => "Closure",
                    Val::Thunk(_, _, _) => "Thunk",
                    Val::Builtin(_) => "Builtin",
                    Val::Continuation(_) => "Continuation",
                    Val::Unit => "Unit",
                    Val::Nil => "Nil",
                };
                self.stack.push(Val::str(ty));
            }
            Instr::HashOf => {
                return Err(VMError::NotImplemented("HashOf".into()));
            }
            Instr::Assert => {
                let msg = self.pop()?;
                let cond = self.pop_bool()?;
                if !cond {
                    let msg_str = msg.as_str().unwrap_or("assertion failed");
                    return Err(VMError::AssertionFailed(msg_str.to_string()));
                }
            }
            Instr::Trace => {
                let v = self.pop()?;
                eprintln!("[trace] {}", v);
                self.stack.push(v);
            }
            Instr::Nop => {}
        }
        Ok(())
    }

    // Helpers

    fn pop(&mut self) -> VMResult<Val> {
        self.stack.pop().ok_or(VMError::StackUnderflow)
    }

    fn peek(&self, n: usize) -> VMResult<&Val> {
        let len = self.stack.len();
        if n >= len {
            return Err(VMError::StackUnderflow);
        }
        Ok(&self.stack[len - 1 - n])
    }

    fn pop_int(&mut self) -> VMResult<i64> {
        self.pop()?.as_int().ok_or(VMError::TypeMismatch { 
            expected: "int", 
            got: "non-int".into() 
        })
    }

    fn pop_bool(&mut self) -> VMResult<bool> {
        self.pop()?.as_bool().ok_or(VMError::TypeMismatch { 
            expected: "bool", 
            got: "non-bool".into() 
        })
    }

    fn binop_int(&mut self, f: fn(i64, i64) -> i64) -> VMResult<()> {
        let b = self.pop_int()?;
        let a = self.pop_int()?;
        self.stack.push(Val::Int(f(a, b)));
        Ok(())
    }

    fn binop_bool(&mut self, f: fn(bool, bool) -> bool) -> VMResult<()> {
        let b = self.pop_bool()?;
        let a = self.pop_bool()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn cmp_int(&mut self, f: fn(i64, i64) -> bool) -> VMResult<()> {
        let b = self.pop_int()?;
        let a = self.pop_int()?;
        self.stack.push(Val::Bool(f(a, b)));
        Ok(())
    }

    fn cmp_op(&mut self, f: fn(&Val, &Val) -> bool) -> VMResult<()> {
        let b = self.pop()?;
        let a = self.pop()?;
        self.stack.push(Val::Bool(f(&a, &b)));
        Ok(())
    }

    fn literal_to_val(&self, lit: Literal) -> Val {
        match lit {
            Literal::Int(n) => Val::Int(n),
            Literal::Str(s) => Val::str(s),
            Literal::Bool(b) => Val::Bool(b),
            Literal::Float(f) => Val::Float(f),
            Literal::Hash(_h) => Val::Unit, // TODO: hash lookup
            Literal::Unit => Val::Unit,
            Literal::Nil => Val::Nil,
        }
    }

    fn call(&mut self, hash: Hash) -> VMResult<()> {
        self.push_frame();
        self.code = Some(hash);
        self.pc = 0;
        Ok(())
    }

    fn push_frame(&mut self) {
        let code = self.code.unwrap_or_else(Hash::unit);
        let frame = Frame::new(code, self.pc, self.env.clone());
        self.frames.push(frame);
    }

    fn builtin(&mut self, op: BuiltinOp) -> VMResult<()> {
        match op {
            BuiltinOp::Print => {
                let v = self.pop()?;
                println!("{}", v);
                self.stack.push(Val::Unit);
            }
            BuiltinOp::IntToStr => {
                let n = self.pop_int()?;
                self.stack.push(Val::str(n.to_string()));
            }
            BuiltinOp::StrToInt => {
                let v = self.pop()?;
                let s = v.as_str().ok_or(VMError::TypeMismatch { 
                    expected: "string", got: format!("{}", v) 
                })?;
                let n: i64 = s.parse().map_err(|_| VMError::TypeMismatch { 
                    expected: "numeric string", got: s.to_string() 
                })?;
                self.stack.push(Val::Int(n));
            }
            BuiltinOp::Abs => {
                let n = self.pop_int()?;
                self.stack.push(Val::Int(n.abs()));
            }
            BuiltinOp::Min => {
                let b = self.pop_int()?;
                let a = self.pop_int()?;
                self.stack.push(Val::Int(a.min(b)));
            }
            BuiltinOp::Max => {
                let b = self.pop_int()?;
                let a = self.pop_int()?;
                self.stack.push(Val::Int(a.max(b)));
            }
            BuiltinOp::Range => {
                let end = self.pop_int()?;
                let start = self.pop_int()?;
                let list: Vec<Val> = (start..end).map(Val::Int).collect();
                self.stack.push(Val::list(list));
            }
            _ => return Err(VMError::NotImplemented(format!("{:?}", op))),
        }
        Ok(())
    }

    // Stats
    pub fn step_count(&self) -> u64 { self.steps }
    pub fn stack_depth(&self) -> usize { self.stack.len() }
    pub fn frame_depth(&self) -> usize { self.frames.len() }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::Instr::{Push, Add, Mul, Gt, MkList, Len, Jump, Halt};
    use crate::store::Store as CodeStore;

    fn run_code(code: Vec<crate::instr::Instr>) -> VMResult<Val> {
        let mut store = CodeStore::new();
        let block = CodeBlock::new(code).with_name("main");
        let hash = store.add_code(block);
        VM::new(&store).run(hash)
    }

    #[test]
    fn test_arithmetic() {
        let result = run_code(vec![
            Push(Literal::Int(10)),
            Push(Literal::Int(3)),
            Add,
            Push(Literal::Int(2)),
            Mul,
            Halt,
        ]).unwrap();
        assert_eq!(result.as_int(), Some(26));
    }

    #[test]
    fn test_comparison() {
        let result = run_code(vec![
            Push(Literal::Int(5)),
            Push(Literal::Int(3)),
            Gt,
            Halt,
        ]).unwrap();
        assert_eq!(result.as_bool(), Some(true));
    }

    #[test]
    fn test_list() {
        let result = run_code(vec![
            Push(Literal::Int(1)),
            Push(Literal::Int(2)),
            Push(Literal::Int(3)),
            MkList(3),
            Len,
            Halt,
        ]).unwrap();
        assert_eq!(result.as_int(), Some(3));
    }

    #[test]
    fn test_jump() {
        let result = run_code(vec![
            Push(Literal::Int(1)),
            Jump(2),           // skip next
            Push(Literal::Int(99)),
            Push(Literal::Int(2)),
            Add,
            Halt,
        ]).unwrap();
        assert_eq!(result.as_int(), Some(3));
    }
}
