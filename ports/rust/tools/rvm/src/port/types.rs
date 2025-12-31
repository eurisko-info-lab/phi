//! Port Type System
//!
//! Types, Kinds, Modules, and Declarations.

/// Type parameters for generic types
#[derive(Debug, Clone, PartialEq)]
pub enum TypeParam {
    Simple(String),
    Kinded(String, Kind),
}

impl TypeParam {
    pub fn name(&self) -> &str {
        match self {
            TypeParam::Simple(n) => n,
            TypeParam::Kinded(n, _) => n,
        }
    }
}

/// Kinds classify types
#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    /// * - the kind of types
    Type,
    /// * -> * - type constructors
    Func(Box<Kind>, Box<Kind>),
}

/// Types in Port
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitives
    Int,
    String,
    Bool,
    Unit,
    
    // References
    Ref(String),
    Var(String),
    
    // Type application: List[Int], Map[String, Int]
    App(Box<Type>, Vec<Type>),
    
    // Structural types
    List(Box<Type>),
    Option(Box<Type>),
    Tuple(Vec<Type>),
    Func(Box<Type>, Box<Type>),
    
    // Higher-kinded
    Higher(String, usize),
}

impl Type {
    pub fn int() -> Self { Type::Int }
    pub fn string() -> Self { Type::String }
    pub fn bool() -> Self { Type::Bool }
    pub fn unit() -> Self { Type::Unit }
    pub fn var(name: impl Into<String>) -> Self { Type::Var(name.into()) }
    pub fn reference(name: impl Into<String>) -> Self { Type::Ref(name.into()) }
    pub fn list(elem: Type) -> Self { Type::List(Box::new(elem)) }
    pub fn option(elem: Type) -> Self { Type::Option(Box::new(elem)) }
    pub fn tuple(elems: Vec<Type>) -> Self { Type::Tuple(elems) }
    pub fn func(from: Type, to: Type) -> Self { Type::Func(Box::new(from), Box::new(to)) }
    pub fn app(ctor: Type, args: Vec<Type>) -> Self { Type::App(Box::new(ctor), args) }
}

/// Field in a record or constructor
#[derive(Debug, Clone, PartialEq)]
pub enum Field {
    Named(String, Type),
    Anon(Type),
}

impl Field {
    pub fn named(name: impl Into<String>, typ: Type) -> Self {
        Field::Named(name.into(), typ)
    }
    
    pub fn anon(typ: Type) -> Self {
        Field::Anon(typ)
    }
    
    pub fn typ(&self) -> &Type {
        match self {
            Field::Named(_, t) => t,
            Field::Anon(t) => t,
        }
    }
}

/// Constructor in a sum type
#[derive(Debug, Clone, PartialEq)]
pub struct Constructor {
    pub name: String,
    pub fields: Vec<Field>,
}

impl Constructor {
    pub fn new(name: impl Into<String>, fields: Vec<Field>) -> Self {
        Constructor { name: name.into(), fields }
    }
    
    pub fn unit(name: impl Into<String>) -> Self {
        Constructor { name: name.into(), fields: vec![] }
    }
}

/// Function parameter
#[derive(Debug, Clone, PartialEq)]
pub enum Param {
    Normal(String, Type),
    Implicit(String, Type),
}

impl Param {
    pub fn normal(name: impl Into<String>, typ: Type) -> Self {
        Param::Normal(name.into(), typ)
    }
    
    pub fn implicit(name: impl Into<String>, typ: Type) -> Self {
        Param::Implicit(name.into(), typ)
    }
    
    pub fn name(&self) -> &str {
        match self {
            Param::Normal(n, _) | Param::Implicit(n, _) => n,
        }
    }
    
    pub fn typ(&self) -> &Type {
        match self {
            Param::Normal(_, t) | Param::Implicit(_, t) => t,
        }
    }
}

/// Declarations in a module
#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    /// Sum type: type Color = Red | Green | Blue
    SumType {
        name: String,
        params: Vec<TypeParam>,
        constructors: Vec<Constructor>,
    },
    
    /// Product type: record Point { x: Int, y: Int }
    ProductType {
        name: String,
        params: Vec<TypeParam>,
        fields: Vec<Field>,
    },
    
    /// Newtype wrapper
    Newtype {
        name: String,
        params: Vec<TypeParam>,
        wrapped: Type,
    },
    
    /// Type alias
    TypeAlias {
        name: String,
        params: Vec<TypeParam>,
        target: Type,
    },
    
    /// Function declaration
    Func {
        name: String,
        params: Vec<Param>,
        ret: Type,
        body: super::expr::Expr,
    },
}

impl Decl {
    pub fn sum_type(name: impl Into<String>, params: Vec<TypeParam>, constructors: Vec<Constructor>) -> Self {
        Decl::SumType { name: name.into(), params, constructors }
    }
    
    pub fn product_type(name: impl Into<String>, params: Vec<TypeParam>, fields: Vec<Field>) -> Self {
        Decl::ProductType { name: name.into(), params, fields }
    }
    
    pub fn newtype(name: impl Into<String>, params: Vec<TypeParam>, wrapped: Type) -> Self {
        Decl::Newtype { name: name.into(), params, wrapped }
    }
    
    pub fn type_alias(name: impl Into<String>, params: Vec<TypeParam>, target: Type) -> Self {
        Decl::TypeAlias { name: name.into(), params, target }
    }
    
    pub fn func(name: impl Into<String>, params: Vec<Param>, ret: Type, body: super::expr::Expr) -> Self {
        Decl::Func { name: name.into(), params, ret, body }
    }
    
    pub fn name(&self) -> &str {
        match self {
            Decl::SumType { name, .. } => name,
            Decl::ProductType { name, .. } => name,
            Decl::Newtype { name, .. } => name,
            Decl::TypeAlias { name, .. } => name,
            Decl::Func { name, .. } => name,
        }
    }
}

/// Module exports
#[derive(Debug, Clone, PartialEq)]
pub enum Export {
    Type(String),
    Func(String),
    All,
}

/// Module imports
#[derive(Debug, Clone, PartialEq)]
pub enum Import {
    Module(String),
    Qualified(String, String),
}

/// A module groups related types and functions
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub exports: Vec<Export>,
    pub imports: Vec<Import>,
    pub decls: Vec<Decl>,
}

impl Module {
    pub fn new(name: impl Into<String>) -> Self {
        Module {
            name: name.into(),
            exports: vec![],
            imports: vec![],
            decls: vec![],
        }
    }
    
    pub fn with_decls(mut self, decls: Vec<Decl>) -> Self {
        self.decls = decls;
        self
    }
    
    pub fn find_decl(&self, name: &str) -> Option<&Decl> {
        self.decls.iter().find(|d| d.name() == name)
    }
    
    pub fn find_func(&self, name: &str) -> Option<&Decl> {
        self.decls.iter().find(|d| matches!(d, Decl::Func { name: n, .. } if n == name))
    }
}

/// Capabilities a port provides
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Capability {
    RecursionSchemes,
    Optics,
    Validation,
    Zipper,
    FreeMonad,
    Cofree,
    Parsing,
    PrettyPrint,
}

/// Complete port specification
#[derive(Debug, Clone, PartialEq)]
pub struct PortSpec {
    pub name: String,
    pub modules: Vec<Module>,
    pub capabilities: Vec<Capability>,
}

impl PortSpec {
    pub fn new(name: impl Into<String>) -> Self {
        PortSpec {
            name: name.into(),
            modules: vec![],
            capabilities: vec![],
        }
    }
    
    pub fn with_modules(mut self, modules: Vec<Module>) -> Self {
        self.modules = modules;
        self
    }
    
    pub fn with_capabilities(mut self, capabilities: Vec<Capability>) -> Self {
        self.capabilities = capabilities;
        self
    }
    
    pub fn find_module(&self, name: &str) -> Option<&Module> {
        self.modules.iter().find(|m| m.name == name)
    }
}
