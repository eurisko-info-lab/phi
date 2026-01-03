//! VM Instructions

use crate::hash::Hash;

/// Instructions for the RosettaVM
#[derive(Clone, Debug)]
pub enum Instr {
    // Stack ops
    Push(Literal),
    Pop,
    Dup,
    Swap,
    Rot,
    Over,

    // Environment
    Load(u32),         // load from env slot
    Store(u32),        // store to env slot
    LoadGlobal(Hash),  // load by hash
    StoreGlobal(Hash), // store by hash

    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Boolean
    Not,
    And,
    Or,

    // Control flow
    Jump(i32),         // relative jump
    JumpIf(i32),       // jump if true
    JumpIfNot(i32),    // jump if false
    Call(Hash),        // call function by hash
    TailCall(Hash),    // tail call
    CallN(Hash, u8),   // call with N args
    Return,
    Halt,

    // Closures
    Closure(Hash, u8), // code hash, num captures
    Apply,             // apply closure on stack
    ApplyN(u8),        // apply with N args
    PartialApply(u8),  // partial application

    // Data
    MkTuple(u8),       // make tuple of N elements
    MkList(u16),       // make list of N elements
    MkRecord(u8),      // make record (keys on stack)
    MkCon(Hash, u8, u8), // type, tag, num fields
    GetField(u8),      // get tuple/record field
    SetField(u8),      // set tuple/record field

    // Pattern matching
    Match(Vec<MatchArm>),
    TestTag(u8),       // test constructor tag
    Unpack(u8),        // unpack constructor fields

    // Lists
    Cons,
    Head,
    Tail,
    IsNil,
    Len,
    Concat,
    Index,

    // Strings
    StrConcat,
    StrLen,
    StrSlice,

    // Effects
    Perform(Hash),     // effect operation hash
    Handle(Hash),      // install handler
    Resume,            // resume continuation

    // Builtins
    Builtin(BuiltinOp),

    // Debug/meta
    Print,
    TypeOf,
    HashOf,
    Assert,
    Trace,
    Nop,
}

/// Literal values in instructions
#[derive(Clone, Debug)]
pub enum Literal {
    Int(i64),
    Str(String),
    Bool(bool),
    Float(f64),
    Hash(Hash),
    Unit,
    Nil,
}

/// Match arm for pattern matching
#[derive(Clone, Debug)]
pub struct MatchArm {
    pub tag: u8,        // constructor tag (or 255 for default)
    pub bindings: u8,   // number of bindings
    pub offset: i32,    // jump offset
}

/// Built-in operations
#[derive(Clone, Debug)]
pub enum BuiltinOp {
    // IO
    ReadLine,
    ReadFile,
    WriteFile,
    Print,
    
    // Math
    Sqrt,
    Abs,
    Min,
    Max,
    Pow,
    
    // Conversion
    IntToStr,
    StrToInt,
    FloatToStr,
    StrToFloat,
    CharToInt,
    IntToChar,

    // Collections
    Map,
    Filter,
    Fold,
    Zip,
    Range,

    // Hash
    HashValue,
    HashEq,

    // Meta
    Eval,
    Quote,
    Typeof,

    // ═══════════════════════════════════════════════════════════════════
    // Network FFI (requires 'network' feature)
    // ═══════════════════════════════════════════════════════════════════
    
    // HTTP client
    HttpGet,      // url, headers -> response
    HttpPost,     // url, headers, body -> response
    HttpPut,      // url, headers, body -> response
    HttpDelete,   // url, headers -> response
    
    // JSON
    JsonParse,    // string -> json value
    JsonStringify, // json value -> string
    JsonGet,      // json, path -> value
    
    // Time
    Now,          // -> timestamp (millis since epoch)
    Sleep,        // millis -> ()
    FormatTime,   // timestamp, format -> string
    ParseTime,    // string, format -> timestamp
    
    // Environment
    GetEnv,       // name -> maybe string
    SetEnv,       // name, value -> ()
    LoadDotEnv,   // path -> ()
    
    // Secrets (secure credential access)
    GetSecret,    // service, key -> maybe string (from keyring)
    
    // Async (requires 'async' feature)
    Spawn,        // thunk -> task_id
    Await,        // task_id -> result
    Timeout,      // millis, thunk -> maybe result
}

/// A compiled code block
#[derive(Clone, Debug)]
pub struct CodeBlock {
    pub hash: Hash,
    pub name: Option<String>,
    pub arity: u8,
    pub locals: u8,
    pub captures: u8,
    pub code: Vec<Instr>,
    pub constants: Vec<Literal>,
    pub source_map: Vec<SourceLoc>,
}

/// Source location for debugging
#[derive(Clone, Debug, Default)]
pub struct SourceLoc {
    pub line: u32,
    pub col: u32,
    pub file: Option<String>,
}

impl CodeBlock {
    pub fn new(code: Vec<Instr>) -> Self {
        let hash = Self::compute_hash(&code);
        CodeBlock {
            hash,
            name: None,
            arity: 0,
            locals: 0,
            captures: 0,
            code,
            constants: Vec::new(),
            source_map: Vec::new(),
        }
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    pub fn with_arity(mut self, arity: u8) -> Self {
        self.arity = arity;
        self
    }

    pub fn with_locals(mut self, locals: u8) -> Self {
        self.locals = locals;
        self
    }

    fn compute_hash(code: &[Instr]) -> Hash {
        use std::hash::{Hash as StdHash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        let mut hasher = DefaultHasher::new();
        format!("{:?}", code).hash(&mut hasher);
        let n = hasher.finish();
        let mut bytes = [0u8; 32];
        bytes[0..8].copy_from_slice(&n.to_le_bytes());
        Hash::new(bytes)
    }

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn get(&self, pc: usize) -> Option<&Instr> {
        self.code.get(pc)
    }
}
