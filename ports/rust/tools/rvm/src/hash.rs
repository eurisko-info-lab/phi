//! Content-addressed hashing using BLAKE3

use std::fmt;

/// A 256-bit content hash
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Hash([u8; 32]);

impl Hash {
    /// Create hash from raw bytes
    pub fn new(bytes: [u8; 32]) -> Self {
        Hash(bytes)
    }

    /// Zero hash (for default/placeholder)
    pub fn zero() -> Self {
        Hash([0u8; 32])
    }

    /// Create hash from raw bytes (alias)
    pub fn from_bytes(bytes: [u8; 32]) -> Self {
        Hash(bytes)
    }

    /// Compute hash of arbitrary bytes
    pub fn of_bytes(data: &[u8]) -> Self {
        Hash(*blake3::hash(data).as_bytes())
    }

    /// Compute hash of a string
    pub fn of_str(s: &str) -> Self {
        Self::of_bytes(s.as_bytes())
    }

    /// Parse hash from hex string
    pub fn from_hex(s: &str) -> Option<Self> {
        if s.len() != 64 {
            return None;
        }
        let mut bytes = [0u8; 32];
        for i in 0..32 {
            bytes[i] = u8::from_str_radix(&s[i*2..i*2+2], 16).ok()?;
        }
        Some(Hash(bytes))
    }

    /// Get short form (first 8 hex chars)
    pub fn short(&self) -> String {
        let full = format!("{}", self);
        full[..8].to_string()
    }

    /// Get raw bytes
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    /// Special hash for unit/empty
    pub fn unit() -> Self {
        Hash([0x11; 32])
    }

    /// Special hash for nil
    pub fn nil() -> Self {
        Hash([0x00; 32])
    }
}

impl fmt::Display for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in &self.0 {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Hash {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.short())
    }
}

/// Trait for types that can be hashed
pub trait Hashable {
    fn hash_bytes(&self) -> Vec<u8>;
    
    fn content_hash(&self) -> Hash {
        Hash::of_bytes(&self.hash_bytes())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_deterministic() {
        let h1 = Hash::of_str("hello");
        let h2 = Hash::of_str("hello");
        assert_eq!(h1, h2);
    }

    #[test]
    fn test_hash_different() {
        let h1 = Hash::of_str("hello");
        let h2 = Hash::of_str("world");
        assert_ne!(h1, h2);
    }

    #[test]
    fn test_short_hash() {
        let h = Hash::of_str("test");
        assert_eq!(h.short().len(), 8);
    }
}
