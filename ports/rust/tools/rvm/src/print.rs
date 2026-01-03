//! Printer for RosettaVM assembly
//!
//! This module is the inverse of parse.rs - it converts instructions back to assembly text.
//! The printer and parser form a Grammar pair: print(parse(s)) ≈ s

use std::collections::HashMap;
use crate::hash::Hash;
use crate::instr::{Instr, Literal, CodeBlock, BuiltinOp};

/// Context for printing with name resolution
pub struct PrintContext {
    /// Map from hash short form to symbolic name
    pub names: HashMap<String, String>,
}

impl PrintContext {
    pub fn new() -> Self {
        PrintContext { names: HashMap::new() }
    }
    
    /// Register a name for a hash
    pub fn register(&mut self, name: &str) {
        let hash = Hash::of_str(name);
        self.names.insert(hash.short(), name.to_string());
    }
    
    /// Look up a name for a hash
    pub fn lookup(&self, hash: &Hash) -> Option<&String> {
        self.names.get(&hash.short())
    }
    
    /// Format a hash, using name if available
    pub fn format_hash(&self, hash: &Hash) -> String {
        if let Some(name) = self.lookup(hash) {
            name.clone()
        } else {
            format!("@{}", hash.short())
        }
    }
}

impl Default for PrintContext {
    fn default() -> Self {
        Self::new()
    }
}

/// Print an instruction to assembly text
pub fn print_instr(instr: &Instr, ctx: &PrintContext) -> String {
    match instr {
        // Stack
        Instr::Push(lit) => format!("push {}", print_literal(lit, ctx)),
        Instr::Pop => "pop".to_string(),
        Instr::Dup => "dup".to_string(),
        Instr::Swap => "swap".to_string(),
        Instr::Rot => "rot".to_string(),
        Instr::Over => "over".to_string(),

        // Env
        Instr::Load(n) => format!("load {}", n),
        Instr::Store(n) => format!("store {}", n),
        Instr::LoadGlobal(h) => format!("loadg {}", ctx.format_hash(h)),
        Instr::StoreGlobal(h) => format!("storeg {}", ctx.format_hash(h)),

        // Arithmetic
        Instr::Add => "add".to_string(),
        Instr::Sub => "sub".to_string(),
        Instr::Mul => "mul".to_string(),
        Instr::Div => "div".to_string(),
        Instr::Mod => "mod".to_string(),
        Instr::Neg => "neg".to_string(),

        // Comparison
        Instr::Eq => "eq".to_string(),
        Instr::Ne => "ne".to_string(),
        Instr::Lt => "lt".to_string(),
        Instr::Le => "le".to_string(),
        Instr::Gt => "gt".to_string(),
        Instr::Ge => "ge".to_string(),

        // Boolean
        Instr::Not => "not".to_string(),
        Instr::And => "and".to_string(),
        Instr::Or => "or".to_string(),

        // Control
        Instr::Jump(n) => format!("jmp {}", n),
        Instr::JumpIf(n) => format!("jt {}", n),
        Instr::JumpIfNot(n) => format!("jf {}", n),
        Instr::Call(h) => format!("call {}", ctx.format_hash(h)),
        Instr::TailCall(h) => format!("tailcall {}", ctx.format_hash(h)),
        Instr::CallN(h, n) => format!("calln {} {}", ctx.format_hash(h), n),
        Instr::Return => "ret".to_string(),
        Instr::Halt => "halt".to_string(),

        // Closures
        Instr::Closure(h, n) => format!("closure {} {}", ctx.format_hash(h), n),
        Instr::Apply => "apply".to_string(),
        Instr::ApplyN(n) => format!("applyn {}", n),
        Instr::PartialApply(n) => format!("partial {}", n),

        // Data
        Instr::MkTuple(n) => format!("mktuple {}", n),
        Instr::MkList(n) => format!("mklist {}", n),
        Instr::MkCon(h, tag, fields) => format!("mkcon {} {} {}", ctx.format_hash(h), tag, fields),
        Instr::GetField(n) => format!("getfield {}", n),
        Instr::Unpack(n) => format!("unpack {}", n),
        Instr::TestTag(n) => format!("testtag {}", n),
        Instr::Index => "index".to_string(),

        // Lists
        Instr::Cons => "cons".to_string(),
        Instr::Head => "head".to_string(),
        Instr::Tail => "tail".to_string(),
        Instr::IsNil => "isnil".to_string(),
        Instr::Len => "len".to_string(),
        Instr::Concat => "concat".to_string(),

        // Strings
        Instr::StrConcat => "strcat".to_string(),
        Instr::StrLen => "strlen".to_string(),
        Instr::StrSlice => "strslice".to_string(),

        // Builtins
        Instr::Print => "print".to_string(),
        Instr::TypeOf => "typeof".to_string(),
        Instr::Assert => "assert".to_string(),
        Instr::Trace => "trace".to_string(),
        Instr::Nop => "nop".to_string(),
        
        Instr::Builtin(op) => match op {
            BuiltinOp::IntToStr => "inttostr".to_string(),
            BuiltinOp::StrToInt => "strtoint".to_string(),
            BuiltinOp::Abs => "abs".to_string(),
            BuiltinOp::Min => "min".to_string(),
            BuiltinOp::Max => "max".to_string(),
            BuiltinOp::Range => "range".to_string(),
            _ => format!("builtin {:?}", op),
        },

        // Catch-all for any new instructions
        _ => format!("; {:?}", instr),
    }
}

/// Print a literal to assembly text
pub fn print_literal(lit: &Literal, ctx: &PrintContext) -> String {
    match lit {
        Literal::Int(n) => n.to_string(),
        Literal::Float(f) => format!("{}", f),
        Literal::Str(s) => format!("\"{}\"", escape_string(s)),
        Literal::Bool(true) => "true".to_string(),
        Literal::Bool(false) => "false".to_string(),
        Literal::Nil => "nil".to_string(),
        Literal::Unit => "unit".to_string(),
        Literal::Hash(h) => ctx.format_hash(h),
    }
}

/// Escape a string for assembly output
fn escape_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '"' => result.push_str("\\\""),
            '\\' => result.push_str("\\\\"),
            '\n' => result.push_str("\\n"),
            '\r' => result.push_str("\\r"),
            '\t' => result.push_str("\\t"),
            _ => result.push(c),
        }
    }
    result
}

/// Print a code block to assembly text
pub fn print_block(block: &CodeBlock, ctx: &PrintContext) -> String {
    let mut output = String::new();
    
    // Function header
    let name = block.name.as_deref().unwrap_or("anonymous");
    if block.arity > 0 {
        let params: Vec<String> = (0..block.arity).map(|i| format!("_{}", i)).collect();
        output.push_str(&format!("fn {}({}) {{\n", name, params.join(", ")));
    } else {
        output.push_str(&format!("fn {}() {{\n", name));
    }
    
    // Instructions
    for instr in &block.code {
        output.push_str(&format!("    {}\n", print_instr(instr, ctx)));
    }
    
    output.push_str("}\n");
    output
}

/// Print multiple code blocks as a module
pub fn print_module(blocks: &[(String, &CodeBlock)], ctx: &PrintContext) -> String {
    let mut output = String::new();
    
    for (name, block) in blocks {
        // Print function header with the provided name
        let arity_part = if block.arity > 0 {
            format!("/{}", block.arity)
        } else {
            String::new()
        };
        output.push_str(&format!("fn {}{} {{\n", name, arity_part));
        
        // Print instructions
        for instr in &block.code {
            output.push_str(&format!("    {}\n", print_instr(instr, ctx)));
        }
        
        output.push_str("}\n\n");
    }
    
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_print_simple_instructions() {
        let ctx = PrintContext::new();
        
        assert_eq!(print_instr(&Instr::Push(Literal::Int(42)), &ctx), "push 42");
        assert_eq!(print_instr(&Instr::Add, &ctx), "add");
        assert_eq!(print_instr(&Instr::Halt, &ctx), "halt");
    }
    
    #[test]
    fn test_print_with_names() {
        let mut ctx = PrintContext::new();
        ctx.register("factorial");
        
        let hash = Hash::of_str("factorial");
        assert_eq!(print_instr(&Instr::Call(hash), &ctx), "call factorial");
    }
    
    #[test]
    fn test_print_string_literal() {
        let ctx = PrintContext::new();
        assert_eq!(
            print_instr(&Instr::Push(Literal::Str("hello".to_string())), &ctx),
            "push \"hello\""
        );
    }
    
    /// Test round-trip: print(parse(s)) ≈ s (Grammar property)
    #[test]
    fn test_roundtrip_grammar_property() {
        use crate::parse::parse_instr;
        
        let ctx = PrintContext::new();
        
        // Test various instruction formats
        let test_cases = [
            "push 42",
            "push 3.14",
            "push true",
            "push nil",
            "push \"hello world\"",
            "add",
            "sub",
            "mul",
            "div",
            "eq",
            "lt",
            "gt",
            "jmp 10",
            "jt 5",
            "jf 3",
            "ret",
            "halt",
            "load 0",
            "store 1",
            "mklist 3",
            "mktuple 2",
            "cons",
            "head",
            "tail",
        ];
        
        for original in test_cases {
            let parsed = parse_instr(original).expect(&format!("Failed to parse: {}", original));
            let printed = print_instr(&parsed, &ctx);
            assert_eq!(printed, original, "Roundtrip failed for: {}", original);
        }
    }
}
