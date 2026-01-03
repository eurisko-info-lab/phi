//! Network FFI for RosettaVM
//!
//! Provides HTTP, JSON, time, and credential operations for autonomous agents.
//! Only compiled when the 'network' feature is enabled.

use crate::value::Val;
use crate::vm::VMResult;
use std::collections::HashMap;

#[cfg(feature = "network")]
use reqwest::blocking::Client;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// HTTP Response as a VM value
#[derive(Clone, Debug)]
pub struct HttpResponse {
    pub status: u16,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

impl HttpResponse {
    pub fn to_val(&self) -> Val {
        Val::Record(vec![
            ("status".into(), Val::Int(self.status as i64)),
            ("headers".into(), headers_to_val(&self.headers)),
            ("body".into(), Val::Bytes(self.body.clone())),
        ])
    }
}

fn headers_to_val(headers: &HashMap<String, String>) -> Val {
    Val::list(
        headers
            .iter()
            .map(|(k, v)| Val::tuple(vec![Val::str(k.clone()), Val::str(v.clone())]))
            .collect(),
    )
}

fn val_to_headers(val: &Val) -> HashMap<String, String> {
    let mut headers = HashMap::new();
    if let Val::List(pairs) = val {
        for pair in pairs.iter() {
            if let Val::Tuple(kv) = pair {
                if kv.len() == 2 {
                    if let (Some(k), Some(v)) = (kv[0].as_str(), kv[1].as_str()) {
                        headers.insert(k.to_string(), v.to_string());
                    }
                }
            }
        }
    }
    headers
}

// ═══════════════════════════════════════════════════════════════════════════
// HTTP Operations
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(feature = "network")]
pub fn http_get(url: &str, headers: &Val) -> VMResult<Val> {
    let client = Client::new();
    let mut req = client.get(url);
    
    for (k, v) in val_to_headers(headers) {
        req = req.header(&k, &v);
    }
    
    let resp = req.send().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP error: {}", e))
    })?;
    
    let status = resp.status().as_u16();
    let headers: HashMap<String, String> = resp
        .headers()
        .iter()
        .filter_map(|(k, v)| {
            v.to_str().ok().map(|v| (k.to_string(), v.to_string()))
        })
        .collect();
    let body = resp.bytes().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP body error: {}", e))
    })?.to_vec();
    
    Ok(HttpResponse { status, headers, body }.to_val())
}

#[cfg(feature = "network")]
pub fn http_post(url: &str, headers: &Val, body: &[u8]) -> VMResult<Val> {
    let client = Client::new();
    let mut req = client.post(url).body(body.to_vec());
    
    for (k, v) in val_to_headers(headers) {
        req = req.header(&k, &v);
    }
    
    let resp = req.send().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP error: {}", e))
    })?;
    
    let status = resp.status().as_u16();
    let resp_headers: HashMap<String, String> = resp
        .headers()
        .iter()
        .filter_map(|(k, v)| {
            v.to_str().ok().map(|v| (k.to_string(), v.to_string()))
        })
        .collect();
    let resp_body = resp.bytes().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP body error: {}", e))
    })?.to_vec();
    
    Ok(HttpResponse { status, headers: resp_headers, body: resp_body }.to_val())
}

#[cfg(feature = "network")]
pub fn http_put(url: &str, headers: &Val, body: &[u8]) -> VMResult<Val> {
    let client = Client::new();
    let mut req = client.put(url).body(body.to_vec());
    
    for (k, v) in val_to_headers(headers) {
        req = req.header(&k, &v);
    }
    
    let resp = req.send().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP error: {}", e))
    })?;
    
    let status = resp.status().as_u16();
    let resp_headers: HashMap<String, String> = resp
        .headers()
        .iter()
        .filter_map(|(k, v)| {
            v.to_str().ok().map(|v| (k.to_string(), v.to_string()))
        })
        .collect();
    let resp_body = resp.bytes().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP body error: {}", e))
    })?.to_vec();
    
    Ok(HttpResponse { status, headers: resp_headers, body: resp_body }.to_val())
}

#[cfg(feature = "network")]
pub fn http_delete(url: &str, headers: &Val) -> VMResult<Val> {
    let client = Client::new();
    let mut req = client.delete(url);
    
    for (k, v) in val_to_headers(headers) {
        req = req.header(&k, &v);
    }
    
    let resp = req.send().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP error: {}", e))
    })?;
    
    let status = resp.status().as_u16();
    let resp_headers: HashMap<String, String> = resp
        .headers()
        .iter()
        .filter_map(|(k, v)| {
            v.to_str().ok().map(|v| (k.to_string(), v.to_string()))
        })
        .collect();
    let body = resp.bytes().map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("HTTP body error: {}", e))
    })?.to_vec();
    
    Ok(HttpResponse { status, headers: resp_headers, body }.to_val())
}

// Stub implementations when network feature is disabled
#[cfg(not(feature = "network"))]
pub fn http_get(_url: &str, _headers: &Val) -> VMResult<Val> {
    Err(crate::vm::VMError::NotImplemented("HTTP requires 'network' feature".into()))
}

#[cfg(not(feature = "network"))]
pub fn http_post(_url: &str, _headers: &Val, _body: &[u8]) -> VMResult<Val> {
    Err(crate::vm::VMError::NotImplemented("HTTP requires 'network' feature".into()))
}

#[cfg(not(feature = "network"))]
pub fn http_put(_url: &str, _headers: &Val, _body: &[u8]) -> VMResult<Val> {
    Err(crate::vm::VMError::NotImplemented("HTTP requires 'network' feature".into()))
}

#[cfg(not(feature = "network"))]
pub fn http_delete(_url: &str, _headers: &Val) -> VMResult<Val> {
    Err(crate::vm::VMError::NotImplemented("HTTP requires 'network' feature".into()))
}

// ═══════════════════════════════════════════════════════════════════════════
// JSON Operations
// ═══════════════════════════════════════════════════════════════════════════

pub fn json_parse(s: &str) -> VMResult<Val> {
    let value: serde_json::Value = serde_json::from_str(s).map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("JSON parse error: {}", e))
    })?;
    Ok(json_to_val(&value))
}

pub fn json_stringify(val: &Val) -> VMResult<String> {
    let json = val_to_json(val);
    serde_json::to_string(&json).map_err(|e| {
        crate::vm::VMError::NotImplemented(format!("JSON stringify error: {}", e))
    })
}

fn json_to_val(json: &serde_json::Value) -> Val {
    use serde_json::Value;
    match json {
        Value::Null => Val::Unit,
        Value::Bool(b) => Val::Bool(*b),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Val::Int(i)
            } else if let Some(f) = n.as_f64() {
                Val::Float(f)
            } else {
                Val::Unit
            }
        }
        Value::String(s) => Val::str(s.clone()),
        Value::Array(arr) => Val::list(arr.iter().map(json_to_val).collect()),
        Value::Object(obj) => Val::Record(
            obj.iter()
                .map(|(k, v)| (k.clone(), json_to_val(v)))
                .collect(),
        ),
    }
}

fn val_to_json(val: &Val) -> serde_json::Value {
    use serde_json::{json, Value, Map};
    match val {
        Val::Unit => Value::Null,
        Val::Bool(b) => json!(b),
        Val::Int(i) => json!(i),
        Val::Float(f) => json!(f),
        Val::Str(s) => json!(s.as_str()),
        Val::List(items) => Value::Array(items.iter().map(val_to_json).collect()),
        Val::Tuple(items) => Value::Array(items.iter().map(val_to_json).collect()),
        Val::Record(fields) => {
            let mut map = Map::new();
            for (k, v) in fields {
                map.insert(k.clone(), val_to_json(v));
            }
            Value::Object(map)
        }
        Val::Bytes(b) => {
            // Encode as base64
            json!(base64_encode(b))
        }
        _ => Value::Null, // Closures, etc. can't be serialized
    }
}

fn base64_encode(data: &[u8]) -> String {
    const ALPHABET: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut result = String::new();
    for chunk in data.chunks(3) {
        let b0 = chunk[0] as usize;
        let b1 = chunk.get(1).copied().unwrap_or(0) as usize;
        let b2 = chunk.get(2).copied().unwrap_or(0) as usize;
        
        result.push(ALPHABET[b0 >> 2] as char);
        result.push(ALPHABET[((b0 & 3) << 4) | (b1 >> 4)] as char);
        if chunk.len() > 1 {
            result.push(ALPHABET[((b1 & 15) << 2) | (b2 >> 6)] as char);
        } else {
            result.push('=');
        }
        if chunk.len() > 2 {
            result.push(ALPHABET[b2 & 63] as char);
        } else {
            result.push('=');
        }
    }
    result
}

// ═══════════════════════════════════════════════════════════════════════════
// Time Operations
// ═══════════════════════════════════════════════════════════════════════════

pub fn now() -> VMResult<Val> {
    let millis = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as i64;
    Ok(Val::Int(millis))
}

#[cfg(feature = "network")]
pub fn sleep(millis: i64) -> VMResult<Val> {
    std::thread::sleep(Duration::from_millis(millis as u64));
    Ok(Val::Unit)
}

#[cfg(not(feature = "network"))]
pub fn sleep(_millis: i64) -> VMResult<Val> {
    // In WASM, sleep is a no-op (can't block)
    Ok(Val::Unit)
}

pub fn format_time(millis: i64, format: &str) -> VMResult<Val> {
    // Simple ISO8601 format
    let secs = millis / 1000;
    let subsec_millis = millis % 1000;
    
    // Very basic formatting - a real implementation would use chrono
    if format == "iso8601" || format.is_empty() {
        // Calculate date components (simplified, ignores leap seconds)
        let days_since_epoch = secs / 86400;
        let time_of_day = secs % 86400;
        let hours = time_of_day / 3600;
        let minutes = (time_of_day % 3600) / 60;
        let seconds = time_of_day % 60;
        
        // Approximate year/month/day (good enough for this use case)
        let year = 1970 + (days_since_epoch / 365);
        let day_of_year = days_since_epoch % 365;
        let month = day_of_year / 30 + 1;
        let day = day_of_year % 30 + 1;
        
        let formatted = format!(
            "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:03}Z",
            year, month, day, hours, minutes, seconds, subsec_millis
        );
        Ok(Val::str(formatted))
    } else {
        Err(crate::vm::VMError::NotImplemented(format!("Unknown time format: {}", format)))
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Environment Operations
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(not(target_arch = "wasm32"))]
pub fn get_env(name: &str) -> VMResult<Val> {
    match std::env::var(name) {
        Ok(value) => Ok(Val::Constructor {
            type_hash: crate::hash::Hash::default(),
            tag: 0, // Just
            fields: vec![Val::str(value)],
        }),
        Err(_) => Ok(Val::Constructor {
            type_hash: crate::hash::Hash::default(),
            tag: 1, // Nothing
            fields: vec![],
        }),
    }
}

#[cfg(target_arch = "wasm32")]
pub fn get_env(_name: &str) -> VMResult<Val> {
    // No environment variables in WASM
    Ok(Val::Constructor {
        type_hash: crate::hash::Hash::default(),
        tag: 1, // Nothing
        fields: vec![],
    })
}

#[cfg(not(target_arch = "wasm32"))]
pub fn set_env(name: &str, value: &str) -> VMResult<Val> {
    std::env::set_var(name, value);
    Ok(Val::Unit)
}

#[cfg(target_arch = "wasm32")]
pub fn set_env(_name: &str, _value: &str) -> VMResult<Val> {
    Ok(Val::Unit)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn load_dotenv(path: &str) -> VMResult<Val> {
    dotenvy::from_filename(path).ok();
    Ok(Val::Unit)
}

#[cfg(target_arch = "wasm32")]
pub fn load_dotenv(_path: &str) -> VMResult<Val> {
    Ok(Val::Unit)
}

// ═══════════════════════════════════════════════════════════════════════════
// Secret/Keyring Operations
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(feature = "network")]
pub fn get_secret(service: &str, key: &str) -> VMResult<Val> {
    use keyring::Entry;
    
    match Entry::new(service, key) {
        Ok(entry) => match entry.get_password() {
            Ok(password) => Ok(Val::Constructor {
                type_hash: crate::hash::Hash::default(),
                tag: 0, // Just
                fields: vec![Val::str(password)],
            }),
            Err(_) => Ok(Val::Constructor {
                type_hash: crate::hash::Hash::default(),
                tag: 1, // Nothing
                fields: vec![],
            }),
        },
        Err(_) => Ok(Val::Constructor {
            type_hash: crate::hash::Hash::default(),
            tag: 1, // Nothing
            fields: vec![],
        }),
    }
}

#[cfg(not(feature = "network"))]
pub fn get_secret(_service: &str, _key: &str) -> VMResult<Val> {
    Err(crate::vm::VMError::NotImplemented("Keyring requires 'network' feature".into()))
}

// ═══════════════════════════════════════════════════════════════════════════
// TESTS
// ═══════════════════════════════════════════════════════════════════════════

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_json_roundtrip() {
        let json_str = r#"{"name": "Phi", "version": 1, "active": true}"#;
        let val = json_parse(json_str).unwrap();
        
        if let Val::Record(fields) = &val {
            assert!(fields.iter().any(|(k, _)| k == "name"));
        } else {
            panic!("Expected record");
        }
        
        let back = json_stringify(&val).unwrap();
        assert!(back.contains("Phi"));
    }

    #[test]
    fn test_now() {
        let Val::Int(millis) = now().unwrap() else { panic!() };
        assert!(millis > 1700000000000); // After 2023
    }

    #[test]
    fn test_format_time() {
        let Val::Str(formatted) = format_time(0, "iso8601").unwrap() else { panic!() };
        assert!(formatted.starts_with("1970-"));
    }
}
