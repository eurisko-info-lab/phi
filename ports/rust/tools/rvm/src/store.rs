//! Content-addressed code store (codebase)

use std::collections::HashMap;
use crate::hash::Hash;
use crate::instr::CodeBlock;
use crate::value::Val;

/// The codebase - content-addressed storage
#[derive(Debug, Default)]
pub struct Store {
    // Code blocks by hash
    code: HashMap<Hash, CodeBlock>,
    // Named aliases -> hash
    names: HashMap<String, Hash>,
    // Name hash -> code hash aliases (for call instructions)
    code_aliases: HashMap<Hash, Hash>,
    // Values/terms by hash
    values: HashMap<Hash, Val>,
    // Type information by hash
    types: HashMap<Hash, TypeInfo>,
    // Dependencies: hash -> hashes it depends on
    deps: HashMap<Hash, Vec<Hash>>,
}

/// Type information for a definition
#[derive(Clone, Debug)]
pub struct TypeInfo {
    pub hash: Hash,
    pub name: Option<String>,
    pub kind: TypeKind,
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Function { params: Vec<Hash>, ret: Hash },
    Data { constructors: Vec<(String, Vec<Hash>)> },
    Record { fields: Vec<(String, Hash)> },
    Effect { operations: Vec<(String, Hash)> },
    Alias(Hash),
    Builtin(String),
}

impl Store {
    pub fn new() -> Self {
        let mut store = Store::default();
        store.register_builtins();
        store
    }

    fn register_builtins(&mut self) {
        // Register built-in types
        let unit_hash = Hash::of_str("builtin:Unit");
        let bool_hash = Hash::of_str("builtin:Bool");
        let int_hash = Hash::of_str("builtin:Int");
        let str_hash = Hash::of_str("builtin:String");
        let list_hash = Hash::of_str("builtin:List");

        self.names.insert("Unit".to_string(), unit_hash);
        self.names.insert("Bool".to_string(), bool_hash);
        self.names.insert("Int".to_string(), int_hash);
        self.names.insert("String".to_string(), str_hash);
        self.names.insert("List".to_string(), list_hash);

        self.types.insert(unit_hash, TypeInfo {
            hash: unit_hash,
            name: Some("Unit".to_string()),
            kind: TypeKind::Builtin("Unit".to_string()),
        });
    }

    // Code operations

    pub fn add_code(&mut self, block: CodeBlock) -> Hash {
        let hash = block.hash;
        if let Some(name) = &block.name {
            self.names.insert(name.clone(), hash);
            // Also create an alias from Hash::of_str(name) to the code hash
            // This allows `call sum_to_n` to work when parsed as Hash::of_str("sum_to_n")
            let name_hash = Hash::of_str(name);
            self.code_aliases.insert(name_hash, hash);
        }
        self.code.insert(hash, block);
        hash
    }

    pub fn get_code(&self, hash: &Hash) -> Option<&CodeBlock> {
        // First try direct lookup, then try via alias
        self.code.get(hash)
            .or_else(|| self.code_aliases.get(hash).and_then(|h| self.code.get(h)))
    }

    pub fn has_code(&self, hash: &Hash) -> bool {
        self.code.contains_key(hash)
    }

    // Name resolution

    pub fn resolve(&self, name: &str) -> Option<Hash> {
        self.names.get(name).copied()
    }

    pub fn alias(&mut self, name: impl Into<String>, hash: Hash) {
        self.names.insert(name.into(), hash);
    }

    pub fn name_of(&self, hash: &Hash) -> Option<&str> {
        self.names.iter()
            .find(|(_, h)| *h == hash)
            .map(|(n, _)| n.as_str())
    }

    // Value operations

    pub fn add_value(&mut self, val: Val) -> Hash {
        let hash = hash_val(&val);
        self.values.insert(hash, val);
        hash
    }

    pub fn get_value(&self, hash: &Hash) -> Option<&Val> {
        self.values.get(hash)
    }

    // Type operations

    pub fn add_type(&mut self, info: TypeInfo) -> Hash {
        let hash = info.hash;
        if let Some(name) = &info.name {
            self.names.insert(name.clone(), hash);
        }
        self.types.insert(hash, info);
        hash
    }

    pub fn get_type(&self, hash: &Hash) -> Option<&TypeInfo> {
        self.types.get(hash)
    }

    // Dependency tracking

    pub fn add_deps(&mut self, hash: Hash, dependencies: Vec<Hash>) {
        self.deps.insert(hash, dependencies);
    }

    pub fn get_deps(&self, hash: &Hash) -> Option<&[Hash]> {
        self.deps.get(hash).map(|v| v.as_slice())
    }

    /// Get transitive closure of dependencies
    pub fn transitive_deps(&self, hash: &Hash) -> Vec<Hash> {
        let mut result = Vec::new();
        let mut stack = vec![*hash];
        let mut seen = std::collections::HashSet::new();

        while let Some(h) = stack.pop() {
            if seen.insert(h) {
                result.push(h);
                if let Some(deps) = self.deps.get(&h) {
                    stack.extend(deps.iter().copied());
                }
            }
        }

        result
    }

    // Stats

    pub fn code_count(&self) -> usize {
        self.code.len()
    }

    pub fn name_count(&self) -> usize {
        self.names.len()
    }

    pub fn type_count(&self) -> usize {
        self.types.len()
    }

    /// List all named definitions
    pub fn list_names(&self) -> impl Iterator<Item = (&str, Hash)> {
        self.names.iter().map(|(n, h)| (n.as_str(), *h))
    }
}

/// Hash a value for content addressing
fn hash_val(val: &Val) -> Hash {
    use std::hash::{Hash as StdHash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hasher = DefaultHasher::new();
    format!("{:?}", val).hash(&mut hasher);
    let n = hasher.finish();
    let mut bytes = [0u8; 32];
    bytes[0..8].copy_from_slice(&n.to_le_bytes());
    Hash::new(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instr::{Instr, Literal};

    #[test]
    fn test_store_code() {
        let mut store = Store::new();
        let block = CodeBlock::new(vec![
            Instr::Push(Literal::Int(42)),
            Instr::Return,
        ]).with_name("answer");

        let hash = store.add_code(block);
        assert!(store.has_code(&hash));
        assert_eq!(store.resolve("answer"), Some(hash));
    }

    #[test]
    fn test_store_alias() {
        let mut store = Store::new();
        let hash = Hash::of_str("test");
        store.alias("myname", hash);
        assert_eq!(store.resolve("myname"), Some(hash));
    }
}
