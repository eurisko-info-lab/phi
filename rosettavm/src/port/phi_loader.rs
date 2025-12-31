//! Phi Language Loader
//!
//! Loads Phi language specifications and creates interpreters.

use std::collections::HashMap;
use super::expr::{Expr, MatchCase};
use super::pattern::Pattern;
use super::val::Val;
use super::env::{Env, standard_env};
use super::eval::{Evaluator, EvalResult, EvalError};
use super::parser;

/// A loaded Phi language
#[derive(Debug, Clone)]
pub struct PhiLanguage {
    pub name: String,
    pub sorts: Vec<String>,
    pub constructors: Vec<PhiConstructor>,
    pub xforms: Vec<PhiXform>,
    pub rules: HashMap<String, Vec<PhiRule>>,
    pub entry_xform: Option<String>,
}

#[derive(Debug, Clone)]
pub struct PhiConstructor {
    pub name: String,
    pub params: Vec<(String, String)>,  // (name, type)
    pub result: String,
}

#[derive(Debug, Clone)]
pub struct PhiXform {
    pub name: String,
    pub from_type: String,
    pub to_type: String,
    pub is_change: bool,
}

#[derive(Debug, Clone)]
pub struct PhiRule {
    pub xform: String,
    pub pattern: String,
    pub result: String,
    pub condition: Option<String>,
}

impl PhiLanguage {
    /// Create an empty language
    pub fn new(name: impl Into<String>) -> Self {
        PhiLanguage {
            name: name.into(),
            sorts: vec![],
            constructors: vec![],
            xforms: vec![],
            rules: HashMap::new(),
            entry_xform: None,
        }
    }
    
    /// Parse a Phi language specification
    pub fn parse(source: &str) -> Result<Self, String> {
        let mut lang = PhiLanguage::new("");
        let mut current_xform: Option<String> = None;
        let mut in_rule_block = false;
        
        for line in source.lines() {
            let line = line.trim();
            
            // Skip empty lines and comments
            if line.is_empty() || line.starts_with("//") {
                continue;
            }
            
            // Language declaration
            if line.starts_with("language ") {
                let name = line.strip_prefix("language ")
                    .and_then(|s| s.strip_suffix(" {"))
                    .or_else(|| line.strip_prefix("language "))
                    .map(|s| s.trim())
                    .unwrap_or("");
                lang.name = name.to_string();
                continue;
            }
            
            // Sort declaration
            if line.starts_with("sort ") {
                let sort = line.strip_prefix("sort ").unwrap().trim();
                lang.sorts.push(sort.to_string());
                continue;
            }
            
            // Constructor block
            if line == "constructor" {
                continue;
            }
            
            // Individual constructor (Name : Type → Type → Result)
            if line.contains(":") && !line.starts_with("xform") && !line.starts_with("change") {
                if let Some((name, sig)) = line.split_once(':') {
                    let name = name.trim();
                    let sig = sig.trim();
                    
                    // Parse signature
                    let parts: Vec<&str> = sig.split('→').collect();
                    let result = parts.last().map(|s| s.trim()).unwrap_or("");
                    let params: Vec<(String, String)> = parts[..parts.len().saturating_sub(1)]
                        .iter()
                        .enumerate()
                        .map(|(i, p)| {
                            let p = p.trim();
                            if let Some((pname, ptype)) = p.split_once(':') {
                                (pname.trim().to_string(), ptype.trim().to_string())
                            } else {
                                (format!("arg{}", i), p.to_string())
                            }
                        })
                        .collect();
                    
                    lang.constructors.push(PhiConstructor {
                        name: name.to_string(),
                        params,
                        result: result.to_string(),
                    });
                }
                continue;
            }
            
            // Xform declaration
            if line.starts_with("xform ") || line.starts_with("change ") {
                let is_change = line.starts_with("change ");
                let rest = if is_change {
                    line.strip_prefix("change ").unwrap()
                } else {
                    line.strip_prefix("xform ").unwrap()
                };
                
                let name = rest.split(':').next().unwrap().trim();
                let sig = rest.split(':').nth(1).unwrap_or("").trim();
                
                // Parse signature (FromType ⇄ ToType or FromType → ToType)
                let (from, to) = if sig.contains('⇄') {
                    let parts: Vec<&str> = sig.split('⇄').collect();
                    (parts.get(0).unwrap_or(&"").trim(), parts.get(1).unwrap_or(&"").trim())
                } else if sig.contains('→') {
                    let parts: Vec<&str> = sig.split('→').collect();
                    (parts.get(0).unwrap_or(&"").trim(), parts.get(1).unwrap_or(&"").trim())
                } else {
                    ("", "")
                };
                
                lang.xforms.push(PhiXform {
                    name: name.to_string(),
                    from_type: from.to_string(),
                    to_type: to.to_string(),
                    is_change,
                });
                
                // Detect entry points
                if name == "Solve" || name == "Eval" || name == "eval" || name == "run" {
                    lang.entry_xform = Some(name.to_string());
                }
                
                current_xform = Some(name.to_string());
                continue;
            }
            
            // Rule block
            if line.starts_with("rule ") {
                let xform_name = line.strip_prefix("rule ")
                    .and_then(|s| s.split('.').next())
                    .or_else(|| line.strip_prefix("rule ").and_then(|s| s.strip_suffix(" {")))
                    .map(|s| s.trim())
                    .unwrap_or("");
                current_xform = Some(xform_name.to_string());
                in_rule_block = true;
                continue;
            }
            
            // Individual rule (pattern ↦ result)
            if line.contains('↦') {
                if let Some((pattern, result)) = line.split_once('↦') {
                    let pattern = pattern.trim().to_string();
                    let result = result.trim().trim_end_matches(',').to_string();
                    
                    // Check for condition (where ...)
                    let (result, condition) = if result.contains("where") {
                        let parts: Vec<&str> = result.splitn(2, "where").collect();
                        (parts[0].trim().to_string(), Some(parts[1].trim().to_string()))
                    } else {
                        (result, None)
                    };
                    
                    if let Some(ref xform) = current_xform {
                        lang.rules.entry(xform.clone()).or_default().push(PhiRule {
                            xform: xform.clone(),
                            pattern,
                            result,
                            condition,
                        });
                    }
                }
                continue;
            }
            
            // Strategy declaration (can be entry point)
            if line.starts_with("strategy ") {
                let name = line.strip_prefix("strategy ")
                    .and_then(|s| s.split_whitespace().next())
                    .unwrap_or("");
                if lang.entry_xform.is_none() {
                    lang.entry_xform = Some(name.to_string());
                }
                continue;
            }
        }
        
        // Default entry point
        if lang.entry_xform.is_none() && !lang.xforms.is_empty() {
            lang.entry_xform = Some(lang.xforms[0].name.clone());
        }
        
        Ok(lang)
    }
    
    /// Get constructor by name
    pub fn get_constructor(&self, name: &str) -> Option<&PhiConstructor> {
        self.constructors.iter().find(|c| c.name == name)
    }
    
    /// Get xform by name
    pub fn get_xform(&self, name: &str) -> Option<&PhiXform> {
        self.xforms.iter().find(|x| x.name == name)
    }
    
    /// Get rules for an xform
    pub fn get_rules(&self, xform: &str) -> &[PhiRule] {
        self.rules.get(xform).map(|v| v.as_slice()).unwrap_or(&[])
    }
}

/// Phi interpreter - executes programs in a loaded Phi language
pub struct PhiInterpreter {
    pub language: PhiLanguage,
    pub evaluator: Evaluator,
    pub env: Env,
    pub program: Vec<Val>,  // Loaded program clauses
}

impl PhiInterpreter {
    /// Create a new interpreter for a language
    pub fn new(language: PhiLanguage) -> Self {
        PhiInterpreter {
            language,
            evaluator: Evaluator::new(),
            env: standard_env(),
            program: vec![],
        }
    }
    
    /// Load a program (e.g., Prolog clauses)
    pub fn load_program(&mut self, source: &str) {
        // Parse based on language type
        if self.language.name.contains("Prolog") || self.language.name == "λProlog" {
            self.load_prolog_program(source);
        } else {
            // Generic: store as string
            self.program.push(Val::str(source));
        }
    }
    
    /// Load Prolog-style clauses
    fn load_prolog_program(&mut self, source: &str) {
        let mut current_clause = String::new();
        
        for line in source.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with("%") {
                continue;
            }
            
            current_clause.push_str(line);
            current_clause.push(' ');
            
            if line.ends_with('.') {
                let clause = current_clause.trim().trim_end_matches('.').to_string();
                if !clause.is_empty() {
                    // Parse clause into structured form
                    let clause_val = self.parse_prolog_clause(&clause);
                    self.program.push(clause_val);
                }
                current_clause.clear();
            }
        }
    }
    
    /// Parse a single Prolog clause
    fn parse_prolog_clause(&self, clause: &str) -> Val {
        // Format: head :- body  or just  head
        if let Some((head, body)) = clause.split_once(":-") {
            Val::con("Clause", vec![
                self.parse_prolog_term(head.trim()),
                self.parse_prolog_goal(body.trim()),
            ])
        } else {
            Val::con("Clause", vec![
                self.parse_prolog_term(clause.trim()),
                Val::con0("True"),
            ])
        }
    }
    
    /// Parse a Prolog term (head of clause or argument)
    fn parse_prolog_term(&self, term: &str) -> Val {
        let term = term.trim();
        
        // Check for compound term: name(args)
        if let Some(lparen) = term.find('(') {
            let name = &term[..lparen];
            let args_str = &term[lparen+1..term.len()-1];  // Remove parens
            let args = self.parse_prolog_args(args_str);
            Val::con("App", vec![
                Val::con("Const", vec![Val::str(name)]),
                Val::list(args),
            ])
        } else if term.starts_with('[') {
            // List
            self.parse_prolog_list(term)
        } else if term.chars().next().map_or(false, |c| c.is_uppercase() || c == '_') {
            // Variable
            Val::con("Var", vec![Val::str(term)])
        } else if let Ok(n) = term.parse::<i64>() {
            // Integer
            Val::con("Const", vec![Val::int(n)])
        } else {
            // Atom/constant
            Val::con("Const", vec![Val::str(term)])
        }
    }
    
    /// Parse Prolog arguments (comma-separated, respecting brackets)
    fn parse_prolog_args(&self, args_str: &str) -> Vec<Val> {
        let mut args = vec![];
        let mut current = String::new();
        let mut depth = 0;
        
        for c in args_str.chars() {
            match c {
                '(' | '[' => { depth += 1; current.push(c); }
                ')' | ']' => { depth -= 1; current.push(c); }
                ',' if depth == 0 => {
                    if !current.trim().is_empty() {
                        args.push(self.parse_prolog_term(&current));
                    }
                    current.clear();
                }
                _ => current.push(c),
            }
        }
        if !current.trim().is_empty() {
            args.push(self.parse_prolog_term(&current));
        }
        
        args
    }
    
    /// Parse a Prolog list
    fn parse_prolog_list(&self, list_str: &str) -> Val {
        let inner = list_str.trim()
            .strip_prefix('[')
            .and_then(|s| s.strip_suffix(']'))
            .unwrap_or("");
        
        if inner.is_empty() {
            return Val::list(vec![]);
        }
        
        // Check for head|tail syntax
        if let Some((head_part, tail_part)) = Self::split_list_at_bar(inner) {
            let head_elems = self.parse_prolog_args(head_part);
            let tail = self.parse_prolog_term(tail_part);
            
            // Build cons chain
            let mut result = tail;
            for elem in head_elems.into_iter().rev() {
                result = Val::con("Cons", vec![elem, result]);
            }
            result
        } else {
            let elems = self.parse_prolog_args(inner);
            Val::list(elems)
        }
    }
    
    /// Split list at | respecting brackets
    fn split_list_at_bar(s: &str) -> Option<(&str, &str)> {
        let mut depth = 0;
        for (i, c) in s.char_indices() {
            match c {
                '(' | '[' => depth += 1,
                ')' | ']' => depth -= 1,
                '|' if depth == 0 => {
                    return Some((&s[..i], &s[i+1..]));
                }
                _ => {}
            }
        }
        None
    }
    
    /// Parse a Prolog goal (body of clause)
    fn parse_prolog_goal(&self, goal: &str) -> Val {
        let goal = goal.trim();
        
        // Split on comma (respecting parens)
        let goals = self.split_prolog_goals(goal);
        
        if goals.len() == 1 {
            self.parse_single_goal(&goals[0])
        } else {
            // Conjunction
            let goal_vals: Vec<Val> = goals.iter()
                .map(|g| self.parse_single_goal(g))
                .collect();
            goal_vals.into_iter().reduce(|a, b| Val::con("And", vec![a, b]))
                .unwrap_or(Val::con0("True"))
        }
    }
    
    /// Split goals at commas, respecting parentheses
    fn split_prolog_goals(&self, s: &str) -> Vec<String> {
        let mut goals = vec![];
        let mut current = String::new();
        let mut depth = 0;
        
        for c in s.chars() {
            match c {
                '(' | '[' => { depth += 1; current.push(c); }
                ')' | ']' => { depth -= 1; current.push(c); }
                ',' if depth == 0 => {
                    if !current.trim().is_empty() {
                        goals.push(current.trim().to_string());
                    }
                    current.clear();
                }
                _ => current.push(c),
            }
        }
        if !current.trim().is_empty() {
            goals.push(current.trim().to_string());
        }
        
        goals
    }
    
    /// Parse a single goal
    fn parse_single_goal(&self, goal: &str) -> Val {
        let goal = goal.trim();
        
        // Comparison operators
        if goal.contains("=<") {
            let parts: Vec<&str> = goal.splitn(2, "=<").collect();
            return Val::con("Call", vec![
                Val::con("App", vec![
                    Val::con("Const", vec![Val::str("=<")]),
                    Val::list(vec![
                        self.parse_prolog_term(parts[0].trim()),
                        self.parse_prolog_term(parts[1].trim()),
                    ])
                ])
            ]);
        }
        
        if goal.contains(">") && !goal.contains("->") {
            let parts: Vec<&str> = goal.splitn(2, ">").collect();
            return Val::con("Call", vec![
                Val::con("App", vec![
                    Val::con("Const", vec![Val::str(">")]),
                    Val::list(vec![
                        self.parse_prolog_term(parts[0].trim()),
                        self.parse_prolog_term(parts[1].trim()),
                    ])
                ])
            ]);
        }
        
        // Regular predicate call
        Val::con("Call", vec![self.parse_prolog_term(goal)])
    }
    
    /// Run a query
    pub fn query(&mut self, query_str: &str) -> EvalResult {
        let query_str = query_str.trim().trim_end_matches('.');
        
        // Parse query
        let query = self.parse_prolog_term(query_str);
        
        // For now, use simple pattern matching
        self.solve_query(&query)
    }
    
    /// Solve a query against loaded program
    fn solve_query(&mut self, query: &Val) -> EvalResult {
        // Extract predicate name and args from query
        if let Val::Con(name, args) = query {
            if name == "App" && args.len() == 2 {
                if let (Val::Con(_, pred_args), Val::List(query_args)) = (&args[0], &args[1]) {
                    if let Some(Val::Str(pred_name)) = pred_args.get(0) {
                        return self.solve_predicate(pred_name, query_args);
                    }
                }
            }
        }
        
        Err(EvalError::Custom(format!("cannot solve: {}", query)))
    }
    
    /// Solve a specific predicate
    fn solve_predicate(&mut self, name: &str, args: &[Val]) -> EvalResult {
        match name {
            // Built-in predicates
            "qsort" | "quicksort" => {
                if args.len() >= 1 {
                    let list = self.val_to_list(&args[0])?;
                    let mut sorted = list;
                    sorted.sort_by(|a, b| {
                        // Extract int from Const wrapper if present
                        let get_int = |v: &Val| -> Option<i64> {
                            match v {
                                Val::Int(n) => Some(*n),
                                Val::Con(name, args) if name == "Const" && args.len() == 1 => {
                                    args[0].as_int()
                                }
                                _ => None
                            }
                        };
                        match (get_int(a), get_int(b)) {
                            (Some(x), Some(y)) => x.cmp(&y),
                            _ => std::cmp::Ordering::Equal
                        }
                    });
                    Ok(Val::list(sorted))
                } else {
                    Err(EvalError::ArityMismatch { expected: 2, got: args.len() })
                }
            }
            
            "append" => {
                if args.len() >= 2 {
                    let list1 = self.val_to_list(&args[0])?;
                    let list2 = self.val_to_list(&args[1])?;
                    let mut result = list1;
                    result.extend(list2);
                    Ok(Val::list(result))
                } else {
                    Err(EvalError::ArityMismatch { expected: 3, got: args.len() })
                }
            }
            
            "length" => {
                if args.len() >= 1 {
                    let list = self.val_to_list(&args[0])?;
                    Ok(Val::int(list.len() as i64))
                } else {
                    Err(EvalError::ArityMismatch { expected: 2, got: args.len() })
                }
            }
            
            "reverse" => {
                if args.len() >= 1 {
                    let mut list = self.val_to_list(&args[0])?;
                    list.reverse();
                    Ok(Val::list(list))
                } else {
                    Err(EvalError::ArityMismatch { expected: 2, got: args.len() })
                }
            }
            
            "member" => {
                if args.len() >= 2 {
                    let list = self.val_to_list(&args[1])?;
                    // Return all members
                    Ok(Val::list(list))
                } else {
                    Err(EvalError::ArityMismatch { expected: 2, got: args.len() })
                }
            }
            
            _ => {
                // Try to find in loaded program
                self.solve_from_program(name, args)
            }
        }
    }
    
    /// Convert Val to list of Vals
    fn val_to_list(&self, val: &Val) -> Result<Vec<Val>, EvalError> {
        match val {
            Val::List(l) => Ok(l.clone()),
            Val::Con(name, args) if name == "Cons" && args.len() == 2 => {
                let mut result = vec![args[0].clone()];
                result.extend(self.val_to_list(&args[1])?);
                Ok(result)
            }
            Val::Con(name, args) if name == "Const" && args.len() == 1 => {
                // Might be a list literal in Prolog syntax
                if let Val::Str(s) = &args[0] {
                    if s.starts_with('[') && s.ends_with(']') {
                        // Parse as list
                        let inner = &s[1..s.len()-1];
                        let parts: Vec<Val> = inner.split(',')
                            .filter_map(|p| p.trim().parse::<i64>().ok())
                            .map(Val::int)
                            .collect();
                        return Ok(parts);
                    }
                }
                Err(EvalError::TypeError("expected list".into()))
            }
            _ => Err(EvalError::TypeError("expected list".into())),
        }
    }
    
    /// Try to solve using loaded program clauses
    fn solve_from_program(&mut self, pred: &str, args: &[Val]) -> EvalResult {
        // For now, return an error for unknown predicates
        Err(EvalError::Custom(format!("unknown predicate: {}", pred)))
    }
}

/// Parse a Phi specification file
pub fn load_phi(source: &str) -> Result<PhiLanguage, String> {
    PhiLanguage::parse(source)
}

/// Create an interpreter for a Phi language
pub fn create_interpreter(lang: PhiLanguage) -> PhiInterpreter {
    PhiInterpreter::new(lang)
}
