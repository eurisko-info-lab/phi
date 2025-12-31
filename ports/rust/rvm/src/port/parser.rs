//! Port Parser
//!
//! Parses Port/Phi syntax into AST.

use super::expr::{Expr, MatchCase};
use super::pattern::{Pattern, Literal};
use super::types::*;

/// Parse error
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

type ParseResult<T> = Result<T, ParseError>;

/// Token types
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Upper(String),  // Uppercase identifier (constructor)
    Int(i64),
    Str(String),
    
    // Keywords
    Let,
    In,
    If,
    Then,
    Else,
    Match,
    With,
    Fun,
    Rec,
    Type,
    And,
    Or,
    True,
    False,
    
    // Symbols
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Arrow,      // ->
    FatArrow,   // =>
    Comma,
    Dot,
    Colon,
    ColonColon, // ::
    Semi,
    Eq,
    EqEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Backslash,  // \
    Pipe,       // |
    Ampersand,  // &
    At,         // @
    Underscore, // _
    PlusPlus,   // ++
    
    Newline,
    Eof,
}

/// Lexer
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    col: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input, pos: 0, line: 1, col: 1 }
    }
    
    fn peek(&self) -> Option<char> {
        self.input[self.pos..].chars().next()
    }
    
    fn peek2(&self) -> Option<char> {
        let mut chars = self.input[self.pos..].chars();
        chars.next();
        chars.next()
    }
    
    fn advance(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.pos += c.len_utf8();
        if c == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(c)
    }
    
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.advance();
            } else if c == '-' && self.peek2() == Some('-') {
                // Line comment
                while let Some(c) = self.peek() {
                    if c == '\n' { break; }
                    self.advance();
                }
            } else if c == '/' && self.peek2() == Some('/') {
                // C-style line comment
                while let Some(c) = self.peek() {
                    if c == '\n' { break; }
                    self.advance();
                }
            } else if c == '%' {
                // Prolog-style comment
                while let Some(c) = self.peek() {
                    if c == '\n' { break; }
                    self.advance();
                }
            } else {
                break;
            }
        }
    }
    
    pub fn next_token(&mut self) -> ParseResult<Token> {
        self.skip_whitespace();
        
        let Some(c) = self.peek() else {
            return Ok(Token::Eof);
        };
        
        // Newline
        if c == '\n' {
            self.advance();
            return Ok(Token::Newline);
        }
        
        // String literal
        if c == '"' {
            return self.lex_string();
        }
        
        // Number
        if c.is_ascii_digit() || (c == '-' && self.peek2().map_or(false, |c| c.is_ascii_digit())) {
            return self.lex_number();
        }
        
        // Identifier or keyword
        if c.is_alphabetic() || c == '_' {
            return self.lex_ident();
        }
        
        // Symbols
        self.advance();
        match c {
            '(' => Ok(Token::LParen),
            ')' => Ok(Token::RParen),
            '[' => Ok(Token::LBracket),
            ']' => Ok(Token::RBracket),
            '{' => Ok(Token::LBrace),
            '}' => Ok(Token::RBrace),
            ',' => Ok(Token::Comma),
            '.' => Ok(Token::Dot),
            ';' => Ok(Token::Semi),
            '@' => Ok(Token::At),
            '\\' => Ok(Token::Backslash),
            '_' => Ok(Token::Underscore),
            
            ':' => {
                if self.peek() == Some(':') {
                    self.advance();
                    Ok(Token::ColonColon)
                } else {
                    Ok(Token::Colon)
                }
            }
            
            '-' => {
                if self.peek() == Some('>') {
                    self.advance();
                    Ok(Token::Arrow)
                } else {
                    Ok(Token::Minus)
                }
            }
            
            '=' => {
                match self.peek() {
                    Some('>') => { self.advance(); Ok(Token::FatArrow) }
                    Some('=') => { self.advance(); Ok(Token::EqEq) }
                    Some('<') => { self.advance(); Ok(Token::Le) }  // =<
                    _ => Ok(Token::Eq)
                }
            }
            
            '!' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::Ne)
                } else {
                    Err(self.error("unexpected '!'"))
                }
            }
            
            '<' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::Le)
                } else {
                    Ok(Token::Lt)
                }
            }
            
            '>' => {
                if self.peek() == Some('=') {
                    self.advance();
                    Ok(Token::Ge)
                } else {
                    Ok(Token::Gt)
                }
            }
            
            '+' => {
                if self.peek() == Some('+') {
                    self.advance();
                    Ok(Token::PlusPlus)
                } else {
                    Ok(Token::Plus)
                }
            }
            
            '*' => Ok(Token::Star),
            '/' => Ok(Token::Slash),
            '%' => Ok(Token::Percent),
            '|' => Ok(Token::Pipe),
            '&' => Ok(Token::Ampersand),
            
            _ => Err(self.error(&format!("unexpected character: '{}'", c))),
        }
    }
    
    fn lex_string(&mut self) -> ParseResult<Token> {
        self.advance(); // opening quote
        let mut s = String::new();
        loop {
            match self.peek() {
                None => return Err(self.error("unterminated string")),
                Some('"') => {
                    self.advance();
                    break;
                }
                Some('\\') => {
                    self.advance();
                    match self.peek() {
                        Some('n') => { self.advance(); s.push('\n'); }
                        Some('t') => { self.advance(); s.push('\t'); }
                        Some('r') => { self.advance(); s.push('\r'); }
                        Some('"') => { self.advance(); s.push('"'); }
                        Some('\\') => { self.advance(); s.push('\\'); }
                        Some(c) => { self.advance(); s.push(c); }
                        None => return Err(self.error("unterminated escape")),
                    }
                }
                Some(c) => {
                    self.advance();
                    s.push(c);
                }
            }
        }
        Ok(Token::Str(s))
    }
    
    fn lex_number(&mut self) -> ParseResult<Token> {
        let negative = if self.peek() == Some('-') {
            self.advance();
            true
        } else {
            false
        };
        
        let mut n: i64 = 0;
        while let Some(c) = self.peek() {
            if c.is_ascii_digit() {
                self.advance();
                n = n * 10 + (c as i64 - '0' as i64);
            } else {
                break;
            }
        }
        
        Ok(Token::Int(if negative { -n } else { n }))
    }
    
    fn lex_ident(&mut self) -> ParseResult<Token> {
        let mut s = String::new();
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' || c == '\'' {
                self.advance();
                s.push(c);
            } else {
                break;
            }
        }
        
        // Check for keywords
        let tok = match s.as_str() {
            "let" => Token::Let,
            "in" => Token::In,
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "match" => Token::Match,
            "with" => Token::With,
            "fun" | "fn" => Token::Fun,
            "rec" => Token::Rec,
            "type" => Token::Type,
            "and" => Token::And,
            "or" => Token::Or,
            "true" => Token::True,
            "false" => Token::False,
            _ => {
                // Check if uppercase (constructor)
                if s.chars().next().map_or(false, |c| c.is_uppercase()) {
                    Token::Upper(s)
                } else {
                    Token::Ident(s)
                }
            }
        };
        
        Ok(tok)
    }
    
    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            line: self.line,
            col: self.col,
        }
    }
}

/// Parser
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peeked: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> ParseResult<Self> {
        let mut lexer = Lexer::new(input);
        let current = lexer.next_token()?;
        Ok(Parser { lexer, current, peeked: None })
    }
    
    fn advance(&mut self) -> ParseResult<Token> {
        let old = std::mem::replace(&mut self.current, 
            if let Some(t) = self.peeked.take() { t } else { self.lexer.next_token()? }
        );
        // Skip newlines in most contexts
        while self.current == Token::Newline {
            self.current = self.lexer.next_token()?;
        }
        Ok(old)
    }
    
    fn peek(&mut self) -> ParseResult<&Token> {
        Ok(&self.current)
    }
    
    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        if self.current == expected {
            self.advance()?;
            Ok(())
        } else {
            Err(self.error(&format!("expected {:?}, got {:?}", expected, self.current)))
        }
    }
    
    fn error(&self, msg: &str) -> ParseError {
        ParseError {
            message: msg.to_string(),
            line: self.lexer.line,
            col: self.lexer.col,
        }
    }
    
    /// Parse a complete expression
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_let_or_lower()
    }
    
    fn parse_let_or_lower(&mut self) -> ParseResult<Expr> {
        match &self.current {
            Token::Let => {
                self.advance()?;
                let is_rec = if self.current == Token::Rec {
                    self.advance()?;
                    true
                } else {
                    false
                };
                
                let name = self.parse_ident()?;
                self.expect(Token::Eq)?;
                let value = self.parse_expr()?;
                self.expect(Token::In)?;
                let body = self.parse_expr()?;
                
                if is_rec {
                    Ok(Expr::let_rec(name, value, body))
                } else {
                    Ok(Expr::let_(name, value, body))
                }
            }
            Token::If => {
                self.advance()?;
                let cond = self.parse_expr()?;
                self.expect(Token::Then)?;
                let then_ = self.parse_expr()?;
                self.expect(Token::Else)?;
                let else_ = self.parse_expr()?;
                Ok(Expr::if_(cond, then_, else_))
            }
            Token::Match => {
                self.advance()?;
                let scrutinee = self.parse_expr()?;
                self.expect(Token::With)?;
                let cases = self.parse_match_cases()?;
                Ok(Expr::match_(scrutinee, cases))
            }
            Token::Fun | Token::Backslash => {
                self.advance()?;
                let param = self.parse_ident()?;
                self.expect(Token::Arrow)?;
                let body = self.parse_expr()?;
                Ok(Expr::lam(param, body))
            }
            _ => self.parse_or()
        }
    }
    
    fn parse_or(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_and()?;
        while self.current == Token::Or || self.current == Token::Pipe {
            self.advance()?;
            let right = self.parse_and()?;
            left = Expr::binop("||", left, right);
        }
        Ok(left)
    }
    
    fn parse_and(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_comparison()?;
        while self.current == Token::And || self.current == Token::Ampersand {
            self.advance()?;
            let right = self.parse_comparison()?;
            left = Expr::binop("&&", left, right);
        }
        Ok(left)
    }
    
    fn parse_comparison(&mut self) -> ParseResult<Expr> {
        let left = self.parse_cons()?;
        let op = match &self.current {
            Token::EqEq => "==",
            Token::Ne => "!=",
            Token::Lt => "<",
            Token::Le => "<=",
            Token::Gt => ">",
            Token::Ge => ">=",
            _ => return Ok(left),
        };
        self.advance()?;
        let right = self.parse_cons()?;
        Ok(Expr::binop(op, left, right))
    }
    
    fn parse_cons(&mut self) -> ParseResult<Expr> {
        let left = self.parse_append()?;
        if self.current == Token::ColonColon {
            self.advance()?;
            let right = self.parse_cons()?;  // Right associative
            Ok(Expr::cons(left, right))
        } else {
            Ok(left)
        }
    }
    
    fn parse_append(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_additive()?;
        while self.current == Token::PlusPlus {
            self.advance()?;
            let right = self.parse_additive()?;
            left = Expr::binop("++", left, right);
        }
        Ok(left)
    }
    
    fn parse_additive(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_multiplicative()?;
        loop {
            let op = match &self.current {
                Token::Plus => "+",
                Token::Minus => "-",
                _ => break,
            };
            self.advance()?;
            let right = self.parse_multiplicative()?;
            left = Expr::binop(op, left, right);
        }
        Ok(left)
    }
    
    fn parse_multiplicative(&mut self) -> ParseResult<Expr> {
        let mut left = self.parse_app()?;
        loop {
            let op = match &self.current {
                Token::Star => "*",
                Token::Slash => "/",
                Token::Percent => "%",
                _ => break,
            };
            self.advance()?;
            let right = self.parse_app()?;
            left = Expr::binop(op, left, right);
        }
        Ok(left)
    }
    
    fn parse_app(&mut self) -> ParseResult<Expr> {
        let mut func = self.parse_atom()?;
        while self.is_atom_start() {
            let arg = self.parse_atom()?;
            func = Expr::app(func, arg);
        }
        Ok(func)
    }
    
    fn is_atom_start(&self) -> bool {
        matches!(self.current, 
            Token::Ident(_) | Token::Upper(_) | Token::Int(_) | Token::Str(_) |
            Token::True | Token::False | Token::LParen | Token::LBracket
        )
    }
    
    fn parse_atom(&mut self) -> ParseResult<Expr> {
        match &self.current {
            Token::Int(n) => {
                let n = *n;
                self.advance()?;
                Ok(Expr::int(n))
            }
            Token::Str(s) => {
                let s = s.clone();
                self.advance()?;
                Ok(Expr::string(s))
            }
            Token::True => {
                self.advance()?;
                Ok(Expr::bool(true))
            }
            Token::False => {
                self.advance()?;
                Ok(Expr::bool(false))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(Expr::var(name))
            }
            Token::Upper(name) => {
                let name = name.clone();
                self.advance()?;
                // Check for constructor application
                if self.current == Token::LParen {
                    self.advance()?;
                    let args = self.parse_comma_separated(|p| p.parse_expr(), Token::RParen)?;
                    Ok(Expr::ctor(name, args))
                } else {
                    Ok(Expr::ctor0(name))
                }
            }
            Token::LParen => {
                self.advance()?;
                if self.current == Token::RParen {
                    self.advance()?;
                    return Ok(Expr::unit());
                }
                let expr = self.parse_expr()?;
                // Check for tuple
                if self.current == Token::Comma {
                    let mut elems = vec![expr];
                    while self.current == Token::Comma {
                        self.advance()?;
                        elems.push(self.parse_expr()?);
                    }
                    self.expect(Token::RParen)?;
                    Ok(Expr::tuple(elems))
                } else {
                    self.expect(Token::RParen)?;
                    Ok(expr)
                }
            }
            Token::LBracket => {
                self.advance()?;
                let elems = self.parse_comma_separated(|p| p.parse_expr(), Token::RBracket)?;
                Ok(Expr::list(elems))
            }
            _ => Err(self.error(&format!("unexpected token: {:?}", self.current))),
        }
    }
    
    fn parse_match_cases(&mut self) -> ParseResult<Vec<MatchCase>> {
        let mut cases = vec![];
        // Optional leading |
        if self.current == Token::Pipe {
            self.advance()?;
        }
        loop {
            let pattern = self.parse_pattern()?;
            let guard = if self.current == Token::If {
                self.advance()?;
                Some(self.parse_expr()?)
            } else {
                None
            };
            self.expect(Token::FatArrow)?;
            let body = self.parse_expr()?;
            
            let mut case = MatchCase::new(pattern, body);
            if let Some(g) = guard {
                case = case.with_guard(g);
            }
            cases.push(case);
            
            if self.current != Token::Pipe {
                break;
            }
            self.advance()?;
        }
        Ok(cases)
    }
    
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        self.parse_cons_pattern()
    }
    
    fn parse_cons_pattern(&mut self) -> ParseResult<Pattern> {
        let left = self.parse_atom_pattern()?;
        if self.current == Token::ColonColon {
            self.advance()?;
            let right = self.parse_cons_pattern()?;
            Ok(Pattern::cons(left, right))
        } else {
            Ok(left)
        }
    }
    
    fn parse_atom_pattern(&mut self) -> ParseResult<Pattern> {
        match &self.current {
            Token::Underscore => {
                self.advance()?;
                Ok(Pattern::wild())
            }
            Token::Int(n) => {
                let n = *n;
                self.advance()?;
                Ok(Pattern::int(n))
            }
            Token::Str(s) => {
                let s = s.clone();
                self.advance()?;
                Ok(Pattern::string(s))
            }
            Token::True => {
                self.advance()?;
                Ok(Pattern::bool(true))
            }
            Token::False => {
                self.advance()?;
                Ok(Pattern::bool(false))
            }
            Token::Ident(name) => {
                let name = name.clone();
                self.advance()?;
                // Check for as-pattern
                if self.current == Token::At {
                    self.advance()?;
                    let inner = self.parse_atom_pattern()?;
                    Ok(Pattern::as_(name, inner))
                } else {
                    Ok(Pattern::var(name))
                }
            }
            Token::Upper(name) => {
                let name = name.clone();
                self.advance()?;
                if self.current == Token::LParen {
                    self.advance()?;
                    let args = self.parse_comma_separated(|p| p.parse_pattern(), Token::RParen)?;
                    Ok(Pattern::ctor(name, args))
                } else {
                    Ok(Pattern::ctor0(name))
                }
            }
            Token::LParen => {
                self.advance()?;
                if self.current == Token::RParen {
                    self.advance()?;
                    return Ok(Pattern::tuple(vec![]));
                }
                let pat = self.parse_pattern()?;
                if self.current == Token::Comma {
                    let mut pats = vec![pat];
                    while self.current == Token::Comma {
                        self.advance()?;
                        pats.push(self.parse_pattern()?);
                    }
                    self.expect(Token::RParen)?;
                    Ok(Pattern::tuple(pats))
                } else {
                    self.expect(Token::RParen)?;
                    Ok(pat)
                }
            }
            Token::LBracket => {
                self.advance()?;
                let pats = self.parse_comma_separated(|p| p.parse_pattern(), Token::RBracket)?;
                Ok(Pattern::list(pats))
            }
            _ => Err(self.error(&format!("unexpected pattern token: {:?}", self.current))),
        }
    }
    
    fn parse_ident(&mut self) -> ParseResult<String> {
        match &self.current {
            Token::Ident(name) => {
                let name = name.clone();
                self.advance()?;
                Ok(name)
            }
            _ => Err(self.error("expected identifier")),
        }
    }
    
    fn parse_comma_separated<T, F>(&mut self, mut parse_item: F, end: Token) -> ParseResult<Vec<T>>
    where F: FnMut(&mut Self) -> ParseResult<T>
    {
        let mut items = vec![];
        if self.current == end {
            self.advance()?;
            return Ok(items);
        }
        items.push(parse_item(self)?);
        while self.current == Token::Comma {
            self.advance()?;
            if self.current == end {
                break;  // Allow trailing comma
            }
            items.push(parse_item(self)?);
        }
        self.expect(end)?;
        Ok(items)
    }
}

/// Parse an expression from a string
pub fn parse_expr(input: &str) -> ParseResult<Expr> {
    let mut parser = Parser::new(input)?;
    parser.parse_expr()
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_int() {
        let expr = parse_expr("42").unwrap();
        assert_eq!(expr, Expr::int(42));
    }
    
    #[test]
    fn test_parse_arith() {
        let expr = parse_expr("1 + 2 * 3").unwrap();
        // Should be 1 + (2 * 3) due to precedence
        assert!(matches!(expr, Expr::BinOp(op, _, _) if op == "+"));
    }
    
    #[test]
    fn test_parse_lambda() {
        let expr = parse_expr("\\x -> x + 1").unwrap();
        assert!(matches!(expr, Expr::Lam(_, _)));
    }
    
    #[test]
    fn test_parse_let() {
        let expr = parse_expr("let x = 5 in x + 1").unwrap();
        assert!(matches!(expr, Expr::Let(_, _, _)));
    }
    
    #[test]
    fn test_parse_list() {
        let expr = parse_expr("[1, 2, 3]").unwrap();
        assert!(matches!(expr, Expr::List(_)));
    }
}
