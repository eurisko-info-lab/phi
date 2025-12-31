//! Simple parser for RosettaVM assembly

use crate::hash::Hash;
use crate::instr::{Instr, Literal, CodeBlock, BuiltinOp, MatchArm};
use crate::store::Store;

/// Parse a .rvm assembly file
pub fn parse_file(source: &str, store: &mut Store) -> Result<Hash, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse_module(store)
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub col: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "parse error at {}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for ParseError {}

struct Parser<'a> {
    source: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Parser { source, pos: 0, line: 1, col: 1 }
    }

    fn parse_module(&mut self, store: &mut Store) -> Result<Hash, ParseError> {
        let mut main_hash = None;

        self.skip_ws();
        while !self.at_end() {
            if self.peek_str("fn ") || self.peek_str("def ") {
                let block = self.parse_function()?;
                let name = block.name.clone();
                let hash = store.add_code(block);
                if name.as_deref() == Some("main") {
                    main_hash = Some(hash);
                }
            } else if self.peek_str("//") || self.peek_str("#") {
                self.skip_line();
            } else if self.peek_str(";") {
                self.skip_line();
            } else {
                return Err(self.error("expected 'fn' or 'def'"));
            }
            self.skip_ws();
        }

        main_hash.ok_or_else(|| self.error("no main function defined"))
    }

    fn parse_function(&mut self) -> Result<CodeBlock, ParseError> {
        // fn name(args) { body }
        self.expect_keyword("fn").or_else(|_| self.expect_keyword("def"))?;
        self.skip_ws();
        
        let name = self.parse_ident()?;
        self.skip_ws();

        let arity = if self.peek_char() == Some('(') {
            self.advance();
            let mut count = 0u8;
            while self.peek_char() != Some(')') && !self.at_end() {
                self.skip_ws();
                if count > 0 {
                    self.expect_char(',')?;
                    self.skip_ws();
                }
                self.parse_ident()?;
                count += 1;
            }
            self.expect_char(')')?;
            count
        } else {
            0
        };

        self.skip_ws();
        self.expect_char('{')?;
        
        let code = self.parse_body()?;
        
        self.expect_char('}')?;

        Ok(CodeBlock::new(code)
            .with_name(name)
            .with_arity(arity))
    }

    fn parse_body(&mut self) -> Result<Vec<Instr>, ParseError> {
        let mut instrs = Vec::new();
        
        self.skip_ws();
        while self.peek_char() != Some('}') && !self.at_end() {
            if self.peek_str("//") || self.peek_str("#") || self.peek_str(";") {
                self.skip_line();
            } else {
                let instr = self.parse_instr()?;
                instrs.push(instr);
            }
            self.skip_ws();
        }

        // Implicit return if last isn't halt/return
        if instrs.is_empty() || !matches!(instrs.last(), Some(Instr::Halt | Instr::Return)) {
            instrs.push(Instr::Halt);
        }

        Ok(instrs)
    }

    fn parse_instr(&mut self) -> Result<Instr, ParseError> {
        let op = self.parse_ident()?;
        self.skip_ws_inline();

        let instr = match op.to_lowercase().as_str() {
            // Stack
            "push" => Instr::Push(self.parse_literal()?),
            "pop" => Instr::Pop,
            "dup" => Instr::Dup,
            "swap" => Instr::Swap,
            "rot" => Instr::Rot,
            "over" => Instr::Over,

            // Env
            "load" => Instr::Load(self.parse_u32()?),
            "store" => Instr::Store(self.parse_u32()?),

            // Arithmetic
            "add" => Instr::Add,
            "sub" => Instr::Sub,
            "mul" => Instr::Mul,
            "div" => Instr::Div,
            "mod" => Instr::Mod,
            "neg" => Instr::Neg,

            // Comparison
            "eq" => Instr::Eq,
            "ne" => Instr::Ne,
            "lt" => Instr::Lt,
            "le" => Instr::Le,
            "gt" => Instr::Gt,
            "ge" => Instr::Ge,

            // Boolean
            "not" => Instr::Not,
            "and" => Instr::And,
            "or" => Instr::Or,

            // Control
            "jump" | "jmp" => Instr::Jump(self.parse_i32()?),
            "jumpif" | "jmpif" | "jt" => Instr::JumpIf(self.parse_i32()?),
            "jumpifnot" | "jmpifnot" | "jf" => Instr::JumpIfNot(self.parse_i32()?),
            "call" => {
                let target = self.parse_ident()?;
                Instr::Call(Hash::of_str(&target))
            }
            "tailcall" => {
                let target = self.parse_ident()?;
                Instr::TailCall(Hash::of_str(&target))
            }
            "ret" | "return" => Instr::Return,
            "halt" => Instr::Halt,

            // Closures
            "closure" => {
                let target = self.parse_ident()?;
                self.skip_ws_inline();
                let captures = self.parse_u8().unwrap_or(0);
                Instr::Closure(Hash::of_str(&target), captures)
            }
            "apply" => Instr::Apply,
            "applyn" => Instr::ApplyN(self.parse_u8()?),

            // Data
            "tuple" | "mktuple" => Instr::MkTuple(self.parse_u8()?),
            "list" | "mklist" => Instr::MkList(self.parse_u16()?),
            "con" | "mkcon" => {
                // con TypeName tag fields
                let ty = self.parse_ident()?;
                self.skip_ws_inline();
                let tag = self.parse_u8()?;
                self.skip_ws_inline();
                let fields = self.parse_u8().unwrap_or(0);
                Instr::MkCon(Hash::of_str(&ty), tag, fields)
            }
            "getfield" | "field" => Instr::GetField(self.parse_u8()?),
            "unpack" => Instr::Unpack(self.parse_u8()?),
            "testtag" => Instr::TestTag(self.parse_u8()?),

            // Lists
            "cons" => Instr::Cons,
            "head" => Instr::Head,
            "tail" => Instr::Tail,
            "isnil" => Instr::IsNil,
            "len" => Instr::Len,
            "concat" => Instr::Concat,
            "index" => Instr::Index,

            // Strings
            "strcat" | "strconcat" => Instr::StrConcat,
            "strlen" => Instr::StrLen,
            "strslice" => Instr::StrSlice,

            // Builtins
            "print" => Instr::Print,
            "typeof" => Instr::TypeOf,
            "assert" => Instr::Assert,
            "trace" => Instr::Trace,
            "inttostr" => Instr::Builtin(BuiltinOp::IntToStr),
            "strtoint" => Instr::Builtin(BuiltinOp::StrToInt),
            "abs" => Instr::Builtin(BuiltinOp::Abs),
            "min" => Instr::Builtin(BuiltinOp::Min),
            "max" => Instr::Builtin(BuiltinOp::Max),
            "range" => Instr::Builtin(BuiltinOp::Range),

            "nop" => Instr::Nop,

            _ => return Err(self.error(&format!("unknown instruction: {}", op))),
        };

        self.skip_to_eol();
        Ok(instr)
    }

    fn parse_literal(&mut self) -> Result<Literal, ParseError> {
        self.skip_ws_inline();
        
        if self.peek_char() == Some('"') {
            return Ok(Literal::Str(self.parse_string()?));
        }

        if self.peek_char() == Some('#') {
            self.advance();
            let hex = self.take_while(|c| c.is_ascii_hexdigit());
            return Ok(Literal::Hash(Hash::from_hex(&hex)
                .ok_or_else(|| self.error("invalid hash"))?));
        }

        // true/false/nil/unit
        if self.peek_str("true") {
            self.advance_n(4);
            return Ok(Literal::Bool(true));
        }
        if self.peek_str("false") {
            self.advance_n(5);
            return Ok(Literal::Bool(false));
        }
        if self.peek_str("nil") {
            self.advance_n(3);
            return Ok(Literal::Nil);
        }
        if self.peek_str("()") || self.peek_str("unit") {
            self.advance_n(if self.peek_str("()") { 2 } else { 4 });
            return Ok(Literal::Unit);
        }

        // Number
        let neg = if self.peek_char() == Some('-') {
            self.advance();
            true
        } else {
            false
        };

        let num_str = self.take_while(|c| c.is_ascii_digit() || c == '.');
        if num_str.is_empty() {
            return Err(self.error("expected literal"));
        }

        if num_str.contains('.') {
            let f: f64 = num_str.parse()
                .map_err(|_| self.error("invalid float"))?;
            Ok(Literal::Float(if neg { -f } else { f }))
        } else {
            let n: i64 = num_str.parse()
                .map_err(|_| self.error("invalid integer"))?;
            Ok(Literal::Int(if neg { -n } else { n }))
        }
    }

    fn parse_string(&mut self) -> Result<String, ParseError> {
        self.expect_char('"')?;
        let mut s = String::new();
        while let Some(c) = self.peek_char() {
            if c == '"' {
                self.advance();
                return Ok(s);
            }
            if c == '\\' {
                self.advance();
                match self.peek_char() {
                    Some('n') => { s.push('\n'); self.advance(); }
                    Some('t') => { s.push('\t'); self.advance(); }
                    Some('r') => { s.push('\r'); self.advance(); }
                    Some('\\') => { s.push('\\'); self.advance(); }
                    Some('"') => { s.push('"'); self.advance(); }
                    Some(c) => { s.push(c); self.advance(); }
                    None => break,
                }
            } else {
                s.push(c);
                self.advance();
            }
        }
        Err(self.error("unterminated string"))
    }

    fn parse_ident(&mut self) -> Result<String, ParseError> {
        let s = self.take_while(|c| c.is_alphanumeric() || c == '_' || c == '-');
        if s.is_empty() {
            Err(self.error("expected identifier"))
        } else {
            Ok(s)
        }
    }

    fn parse_u8(&mut self) -> Result<u8, ParseError> {
        let s = self.take_while(|c| c.is_ascii_digit());
        s.parse().map_err(|_| self.error("expected u8"))
    }

    fn parse_u16(&mut self) -> Result<u16, ParseError> {
        let s = self.take_while(|c| c.is_ascii_digit());
        s.parse().map_err(|_| self.error("expected u16"))
    }

    fn parse_u32(&mut self) -> Result<u32, ParseError> {
        let s = self.take_while(|c| c.is_ascii_digit());
        s.parse().map_err(|_| self.error("expected u32"))
    }

    fn parse_i32(&mut self) -> Result<i32, ParseError> {
        let neg = self.peek_char() == Some('-');
        if neg { self.advance(); }
        let s = self.take_while(|c| c.is_ascii_digit());
        let n: i32 = s.parse().map_err(|_| self.error("expected i32"))?;
        Ok(if neg { -n } else { n })
    }

    // Helpers

    fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn peek_char(&self) -> Option<char> {
        self.source[self.pos..].chars().next()
    }

    fn peek_str(&self, s: &str) -> bool {
        self.source[self.pos..].starts_with(s)
    }

    fn advance(&mut self) {
        if let Some(c) = self.peek_char() {
            self.pos += c.len_utf8();
            if c == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        }
    }

    fn advance_n(&mut self, n: usize) {
        for _ in 0..n {
            self.advance();
        }
    }

    fn take_while(&mut self, f: impl Fn(char) -> bool) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek_char() {
            if f(c) {
                s.push(c);
                self.advance();
            } else {
                break;
            }
        }
        s
    }

    fn skip_ws(&mut self) {
        while let Some(c) = self.peek_char() {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_ws_inline(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == ' ' || c == '\t' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_line(&mut self) {
        while let Some(c) = self.peek_char() {
            self.advance();
            if c == '\n' { break; }
        }
    }

    fn skip_to_eol(&mut self) {
        while let Some(c) = self.peek_char() {
            if c == '\n' || c == ';' || c == '#' {
                break;
            }
            if c == '/' && self.peek_str("//") {
                break;
            }
            if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
        // Skip comment if present
        if self.peek_str("//") || self.peek_str("#") || self.peek_str(";") {
            self.skip_line();
        }
    }

    fn expect_char(&mut self, expected: char) -> Result<(), ParseError> {
        if self.peek_char() == Some(expected) {
            self.advance();
            Ok(())
        } else {
            Err(self.error(&format!("expected '{}'", expected)))
        }
    }

    fn expect_keyword(&mut self, kw: &str) -> Result<(), ParseError> {
        if self.peek_str(kw) {
            self.advance_n(kw.len());
            Ok(())
        } else {
            Err(self.error(&format!("expected '{}'", kw)))
        }
    }

    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            line: self.line,
            col: self.col,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple() {
        let source = r#"
            fn main() {
                push 42
                halt
            }
        "#;
        let mut store = Store::new();
        let hash = parse_file(source, &mut store).unwrap();
        assert!(store.has_code(&hash));
    }

    #[test]
    fn test_parse_arithmetic() {
        let source = r#"
            fn main() {
                push 10
                push 20
                add
                halt
            }
        "#;
        let mut store = Store::new();
        parse_file(source, &mut store).unwrap();
    }

    #[test]
    fn test_parse_string() {
        let source = r#"
            fn main() {
                push "hello world"
                print
                halt
            }
        "#;
        let mut store = Store::new();
        parse_file(source, &mut store).unwrap();
    }
}
