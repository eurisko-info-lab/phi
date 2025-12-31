//! RosettaVM: A content-addressed virtual machine
//!
//! Bootstrap implementation in Rust for executing Port/Phi programs.
//! 
//! Inspired by Unison's content-addressed code model:
//! - Every function/term identified by BLAKE3 hash
//! - Names are aliases for hashes (no versioning problems)
//! - Stack-based execution with closures and ADTs

pub mod hash;
pub mod value;
pub mod instr;
pub mod store;
pub mod vm;
pub mod parse;
pub mod compile;
pub mod port;  // Port of Port - the real Phi interpreter

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    match args.get(1).map(|s| s.as_str()) {
        Some("run") => {
            let file = args.get(2).expect("Usage: rosettavm run <file.rvm>");
            run_file(file);
        }
        Some("repl") => {
            repl();
        }
        Some("hash") => {
            let input = args.get(2).expect("Usage: rosettavm hash <file|string>");
            if std::path::Path::new(input).exists() {
                let content = fs::read(input).expect("Cannot read file");
                let h = hash::Hash::of_bytes(&content);
                println!("{}", h);
            } else {
                let h = hash::Hash::of_str(input);
                println!("{}", h);
            }
        }
        Some("eval") => {
            // Quick expression evaluation from command line
            let expr = args.get(2..).map(|s| s.join(" ")).unwrap_or_default();
            eval_expr(&expr);
        }
        Some("test") => {
            run_tests();
        }
        Some("phi") => {
            // Universal interpreter: rvm phi <spec.phi> <source> [-- <query>]
            // Example: rvm phi examples/λProlog.phi quicksort.pl -- "qsort([5,4,8,2,4,1], X)."
            run_phi_interpreter(&args[2..]);
        }
        _ => {
            eprintln!("RosettaVM - Content-addressed virtual machine");
            eprintln!();
            eprintln!("Usage:");
            eprintln!("  rvm run <file.rvm>           Run assembly file");
            eprintln!("  rvm repl                     Interactive REPL");
            eprintln!("  rvm hash <input>             Compute BLAKE3 hash");
            eprintln!("  rvm eval <expr>              Evaluate expression");
            eprintln!("  rvm test                     Run built-in tests");
            eprintln!();
            eprintln!("Universal Interpreter:");
            eprintln!("  rvm phi <spec.phi> <source> [-- <query>]");
            eprintln!();
            eprintln!("Examples:");
            eprintln!("  rvm phi λProlog.phi prog.pl -- \"append([1,2],[3],X).\"");
            eprintln!("  rvm phi stlc.phi terms.stlc -- \"typecheck(lam x.x)\"");
            eprintln!("  rvm phi calc.phi expr.calc");
        }
    }
}

/// Universal interpreter: load a Phi language spec and run source files
fn run_phi_interpreter(args: &[String]) {
    if args.len() < 2 {
        eprintln!("Usage: rvm phi <spec.phi> <source> [-- <query>]");
        eprintln!();
        eprintln!("The spec.phi file defines a language (grammar + transformations).");
        eprintln!("The source file is parsed and interpreted using that spec.");
        eprintln!("An optional query runs against the loaded program.");
        std::process::exit(1);
    }
    
    let spec_path = &args[0];
    let source_path = &args[1];
    
    // Find query after "--"
    let query = args.iter()
        .position(|a| a == "--")
        .map(|i| args[i+1..].join(" "));
    
    // Read files
    let spec_source = fs::read_to_string(spec_path)
        .unwrap_or_else(|e| {
            eprintln!("Cannot read spec file '{}': {}", spec_path, e);
            std::process::exit(1);
        });
    
    let source = fs::read_to_string(source_path)
        .unwrap_or_else(|e| {
            eprintln!("Cannot read source file '{}': {}", source_path, e);
            std::process::exit(1);
        });
    
    // Use the real Port interpreter!
    use port::phi_loader;
    
    // Parse the Phi spec
    let lang = match phi_loader::load_phi(&spec_source) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Error parsing spec: {}", e);
            std::process::exit(1);
        }
    };
    
    println!("📦 Loaded language: {}", lang.name);
    println!("   {} sorts, {} constructors, {} xforms", 
        lang.sorts.len(), 
        lang.constructors.len(),
        lang.xforms.len());
    if let Some(ref entry) = lang.entry_xform {
        println!("   Entry xform: {}", entry);
    }
    
    // Create interpreter
    let mut interp = phi_loader::create_interpreter(lang);
    
    // Load the program
    println!("📄 Loading program ({} bytes)...", source.len());
    interp.load_program(&source);
    println!("   Loaded {} clauses/terms", interp.program.len());
    
    // If there's a query, run it
    if let Some(q) = query {
        println!();
        println!("❓ Query: {}", q.trim());
        println!();
        
        match interp.query(&q) {
            Ok(result) => {
                println!("✓ Solution found:");
                println!("  X = {}", result);
            }
            Err(e) => {
                println!("❌ Error: {}", e);
            }
        }
    } else {
        // Show loaded program summary
        println!();
        println!("▶️  Program loaded. Use -- <query> to run a query.");
        println!();
        for (i, clause) in interp.program.iter().take(5).enumerate() {
            println!("   {}. {}", i + 1, clause);
        }
        if interp.program.len() > 5 {
            println!("   ... and {} more", interp.program.len() - 5);
        }
    }
}

// Old mock implementation removed - now using real Port interpreter in port::phi_loader

fn run_file(path: &str) {
    let source = fs::read_to_string(path).expect("Cannot read file");
    let mut store = store::Store::new();
    
    match parse::parse_file(&source, &mut store) {
        Ok(main_hash) => {
            let mut machine = vm::VM::new(&store);
            match machine.run(main_hash) {
                Ok(result) => println!("{}", result),
                Err(e) => eprintln!("Runtime error: {}", e),
            }
        }
        Err(e) => eprintln!("Parse error: {}", e),
    }
}

fn eval_expr(expr: &str) {
    use compile::Compiler;
    
    // Simple expression parser (just numbers and arithmetic for now)
    let ast = parse_simple_expr(expr);
    let mut store = store::Store::new();
    let hash = Compiler::compile_to_store(&ast, &mut store);
    
    let mut machine = vm::VM::new(&store);
    match machine.run(hash) {
        Ok(result) => println!("{}", result),
        Err(e) => eprintln!("Error: {}", e),
    }
}

fn parse_simple_expr(s: &str) -> compile::Expr {
    // Very basic: just parse integers for now
    use compile::Expr;
    let s = s.trim();
    if let Ok(n) = s.parse::<i64>() {
        Expr::int(n)
    } else if s.starts_with('[') && s.ends_with(']') {
        // Simple list: [1, 2, 3]
        let inner = &s[1..s.len()-1];
        let elems: Vec<_> = inner.split(',')
            .map(|p| parse_simple_expr(p.trim()))
            .collect();
        Expr::list(elems)
    } else {
        Expr::str(s.to_string())
    }
}

fn repl() {
    use std::io::{self, Write, BufRead};
    
    let mut store = store::Store::new();
    let stdin = io::stdin();
    
    println!("RosettaVM REPL (type :help for commands, :quit to exit)");
    
    loop {
        print!("λ> ");
        io::stdout().flush().unwrap();
        
        let mut line = String::new();
        if stdin.lock().read_line(&mut line).unwrap() == 0 {
            break;
        }
        
        let line = line.trim();
        
        match line {
            ":quit" | ":q" => break,
            ":help" | ":h" => {
                println!(":quit    Exit REPL");
                println!(":names   Show named definitions");
                println!(":clear   Clear store");
                println!("<expr>   Evaluate expression");
            }
            ":names" => {
                for (name, hash) in store.list_names() {
                    println!("  {} -> {}", name, hash.short());
                }
            }
            ":clear" => {
                store = store::Store::new();
                println!("Store cleared.");
            }
            "" => continue,
            _ => {
                let ast = parse_simple_expr(line);
                let hash = compile::Compiler::compile_to_store(&ast, &mut store);
                let mut machine = vm::VM::new(&store);
                match machine.run(hash) {
                    Ok(result) => println!("{}", result),
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
        }
    }
}

fn run_tests() {
    println!("Running RosettaVM tests...\n");
    
    let mut store = store::Store::new();
    let mut passed = 0;
    let mut failed = 0;
    
    // Test 1: Basic arithmetic
    {
        let expr = compile::Expr::int(2).add(compile::Expr::int(3)).mul(compile::Expr::int(4));
        let hash = compile::Compiler::compile_to_store(&expr, &mut store);
        let result = vm::VM::new(&store).run(hash);
        match result {
            Ok(v) if v.as_int() == Some(20) => {
                println!("✓ Test 1: arithmetic (2+3)*4 = 20");
                passed += 1;
            }
            _ => {
                println!("✗ Test 1: arithmetic failed");
                failed += 1;
            }
        }
    }
    
    // Test 2: Conditional
    {
        let expr = compile::Expr::if_(
            compile::Expr::int(5).lt(compile::Expr::int(10)),
            compile::Expr::str("yes"),
            compile::Expr::str("no")
        );
        let hash = compile::Compiler::compile_to_store(&expr, &mut store);
        let result = vm::VM::new(&store).run(hash);
        match result {
            Ok(v) if v.as_str() == Some("yes") => {
                println!("✓ Test 2: conditional if 5<10 then yes else no");
                passed += 1;
            }
            _ => {
                println!("✗ Test 2: conditional failed");
                failed += 1;
            }
        }
    }
    
    // Test 3: Let binding
    {
        let expr = compile::Expr::let_(
            "x", 
            compile::Expr::int(100),
            compile::Expr::var("x").add(compile::Expr::int(1))
        );
        let hash = compile::Compiler::compile_to_store(&expr, &mut store);
        let result = vm::VM::new(&store).run(hash);
        match result {
            Ok(v) if v.as_int() == Some(101) => {
                println!("✓ Test 3: let x = 100 in x + 1 = 101");
                passed += 1;
            }
            _ => {
                println!("✗ Test 3: let binding failed");
                failed += 1;
            }
        }
    }
    
    // Test 4: Lists
    {
        let expr = compile::Expr::list(vec![
            compile::Expr::int(1),
            compile::Expr::int(2),
            compile::Expr::int(3),
        ]);
        let hash = compile::Compiler::compile_to_store(&expr, &mut store);
        let result = vm::VM::new(&store).run(hash);
        match result {
            Ok(v) if v.as_list().map(|l| l.len()) == Some(3) => {
                println!("✓ Test 4: list [1, 2, 3] has length 3");
                passed += 1;
            }
            _ => {
                println!("✗ Test 4: list failed");
                failed += 1;
            }
        }
    }
    
    // Test 5: Parse and run assembly
    {
        let source = r#"
            fn main() {
                push 7
                push 6
                mul
                halt
            }
        "#;
        let mut test_store = store::Store::new();
        match parse::parse_file(source, &mut test_store) {
            Ok(hash) => {
                let result = vm::VM::new(&test_store).run(hash);
                match result {
                    Ok(v) if v.as_int() == Some(42) => {
                        println!("✓ Test 5: assembly 7*6 = 42");
                        passed += 1;
                    }
                    _ => {
                        println!("✗ Test 5: assembly execution failed");
                        failed += 1;
                    }
                }
            }
            Err(e) => {
                println!("✗ Test 5: parse failed: {}", e);
                failed += 1;
            }
        }
    }
    
    // Test 6: Content addressing
    {
        let h1 = hash::Hash::of_str("hello");
        let h2 = hash::Hash::of_str("hello");
        let h3 = hash::Hash::of_str("world");
        if h1 == h2 && h1 != h3 {
            println!("✓ Test 6: content addressing (same content = same hash)");
            passed += 1;
        } else {
            println!("✗ Test 6: content addressing failed");
            failed += 1;
        }
    }
    
    println!("\n{} passed, {} failed", passed, failed);
    if failed > 0 {
        std::process::exit(1);    }
}