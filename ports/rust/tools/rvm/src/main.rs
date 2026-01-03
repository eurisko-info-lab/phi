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
pub mod cuda;  // CUDA GPU backend
pub mod cuda_codegen;  // Generic CUDA code generator
pub mod parallel;  // Rayon parallel execution
pub mod phi_compiler;  // Phi to RVM compiler
pub mod ffi;  // Network FFI for autonomous agents

use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::thread;
use std::time::Duration;

// ============================================================================
// Vector4: Recursive Self-Deploy Daemon Mode
// ============================================================================

/// State for Community Management daemon
#[derive(Debug, Clone)]
struct CMState {
    source: String,
    cycle_count: u64,
    last_modified: u64,
}

/// Load current state from specs/cm-current.phi
fn load_current_state() -> Result<CMState, String> {
    let path = "specs/cm-current.phi";
    let source = fs::read_to_string(path)
        .map_err(|e| format!("Failed to load {}: {}", path, e))?;
    Ok(CMState {
        source,
        cycle_count: 0,
        last_modified: std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0),
    })
}

/// Analyze state and propose self-rewrite if beneficial
fn self_rewrite_proposal(state: &CMState) -> Option<(String, String)> {
    // Stub: In a real implementation, this would analyze metrics,
    // community feedback, and propose improvements to the source.
    // For now, return None (no proposals)
    let _ = state;
    None
}

/// Deploy new source: write file, git commit, git push, exec new version
fn deploy_self(new_source: &str, commit_msg: &str) -> Result<(), String> {
    let path = "specs/cm-current.phi";
    
    // Write new source
    fs::write(path, new_source)
        .map_err(|e| format!("Failed to write {}: {}", path, e))?;
    
    // Git add
    let status = Command::new("git")
        .args(["add", path])
        .status()
        .map_err(|e| format!("Git add failed: {}", e))?;
    if !status.success() {
        return Err("Git add failed".to_string());
    }
    
    // Git commit with Phi-Daemon author
    let status = Command::new("git")
        .args([
            "-c", "user.name=Phi-Daemon",
            "-c", "user.email=daemon@phi.local",
            "commit", "-m", commit_msg
        ])
        .status()
        .map_err(|e| format!("Git commit failed: {}", e))?;
    if !status.success() {
        return Err("Git commit failed".to_string());
    }
    
    // Git push
    let status = Command::new("git")
        .args(["push"])
        .status()
        .map_err(|e| format!("Git push failed: {}", e))?;
    if !status.success() {
        return Err("Git push failed".to_string());
    }
    
    // Exec new version (replaces current process)
    println!("[vector4] Executing new version...");
    let err = Command::new("rosettavm")
        .args(["cuda", "specs/cm-current.phi", "--vector4"])
        .exec(); // Note: exec() only returns on error
    
    Err(format!("Exec failed: {:?}", err))
}

/// Run one cycle of community management
fn run_one_cycle_of_cm(state: &mut CMState) {
    state.cycle_count += 1;
    println!("[vector4] CM cycle #{}: Running community management...", state.cycle_count);
    
    // Stub: In a real implementation, this would:
    // - Check for new community posts/issues
    // - Generate responses or content
    // - Update metrics and state
    // For now, just log
    println!("[vector4] CM cycle #{}: Generated sample post (stub)", state.cycle_count);
}

/// Check if kill switch is active
fn kill_switch_active() -> bool {
    Path::new("/kill.switch").exists()
}

/// Run the Vector4 self-deploy daemon loop
fn run_vector4_daemon() {
    println!("╔═══════════════════════════════════════════════════════════════╗");
    println!("║          Phi Vector4 - Recursive Self-Deploy Daemon          ║");
    println!("╠═══════════════════════════════════════════════════════════════╣");
    println!("║ Kill switch: /kill.switch                                    ║");
    println!("║ State file:  specs/cm-current.phi                            ║");
    println!("║ Cycle time:  3600 seconds (1 hour)                           ║");
    println!("╚═══════════════════════════════════════════════════════════════╝");
    println!();
    
    loop {
        // 1. Check kill switch
        if kill_switch_active() {
            println!("[vector4] Kill switch detected at /kill.switch");
            println!("[vector4] Exiting gracefully...");
            break;
        }
        
        // 2. Load current state
        let mut state = match load_current_state() {
            Ok(s) => {
                println!("[vector4] Loaded state ({} bytes)", s.source.len());
                s
            }
            Err(e) => {
                eprintln!("[vector4] Warning: {}", e);
                eprintln!("[vector4] Creating default state...");
                CMState {
                    source: String::new(),
                    cycle_count: 0,
                    last_modified: 0,
                }
            }
        };
        
        // 3. Check for self-rewrite proposals
        if let Some((new_source, commit_msg)) = self_rewrite_proposal(&state) {
            println!("[vector4] Proposing self-evolution: {}", commit_msg);
            match deploy_self(&new_source, &commit_msg) {
                Ok(()) => {
                    // deploy_self execs new process, so we shouldn't reach here
                    println!("[vector4] Deploy succeeded (unexpected return)");
                }
                Err(e) => {
                    eprintln!("[vector4] Deploy failed: {}", e);
                    // Continue with current version
                }
            }
        }
        
        // 4. Run one cycle of community management
        run_one_cycle_of_cm(&mut state);
        
        // 5. Sleep for 1 hour
        println!("[vector4] Sleeping for 3600 seconds...");
        thread::sleep(Duration::from_secs(3600));
    }
    
    println!("[vector4] Daemon terminated.");
}

// Trait extension for exec (Unix-like behavior)
#[cfg(unix)]
use std::os::unix::process::CommandExt;

#[cfg(not(unix))]
trait CommandExt {
    fn exec(&mut self) -> std::io::Error;
}

#[cfg(not(unix))]
impl CommandExt for Command {
    fn exec(&mut self) -> std::io::Error {
        // On non-Unix, spawn and exit
        match self.spawn() {
            Ok(_) => std::process::exit(0),
            Err(e) => e,
        }
    }
}

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
        Some("bench") => {
            // Run CPU vs GPU benchmarks
            run_benchmarks(&args[2..]);
        }
        Some("parallel") => {
            // Run parallel CPU benchmarks with Rayon
            parallel::ParallelBench::run_all();
        }
        Some("cuda") => {
            // Compile RVM to CUDA
            let file = args.get(2).expect("Usage: rosettavm cuda <file.rvm>");
            compile_to_cuda(file);
        }
        Some("compile") => {
            // Compile Phi to RVM
            if args.len() < 3 {
                eprintln!("Usage: rvm compile <expr> | rvm compile --file <file.phi>");
                return;
            }
            if args.get(2).map(|s| s.as_str()) == Some("--file") {
                let file = args.get(3).expect("Expected filename");
                compile_phi_file(file);
            } else {
                let expr = args[2..].join(" ");
                compile_phi_expr(&expr);
            }
        }
        Some("phi") => {
            // Universal interpreter: rvm phi <spec.phi> <source> [-- <query>]
            // Example: rvm phi examples/λProlog.phi quicksort.pl -- "qsort([5,4,8,2,4,1], X)."
            run_phi_interpreter(&args[2..]);
        }
        Some("--vector4") => {
            // Recursive self-deploy daemon mode
            run_vector4_daemon();
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
            eprintln!("Compilation & Execution:");
            eprintln!("  rvm compile <expr>           Compile Phi expression to RVM");
            eprintln!("  rvm compile --file <f.phi>   Compile Phi file to RVM");
            eprintln!("  rvm cuda <file.rvm>          Compile RVM to CUDA");
            eprintln!();
            eprintln!("Benchmarks:");
            eprintln!("  rvm bench [tree|sum]         CPU vs GPU benchmark");
            eprintln!("  rvm parallel                 Parallel CPU benchmark (Rayon)");
            eprintln!();
            eprintln!("Universal Interpreter:");
            eprintln!("  rvm phi <spec.phi> <source> [-- <query>]");
            eprintln!();
            eprintln!("Daemon Mode:");
            eprintln!("  rvm --vector4                Recursive self-deploy daemon");
            eprintln!("                               (kill switch: /kill.switch)");
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
            let mut machine = vm::VM::new(&store).debug(std::env::var("RVM_DEBUG").is_ok());
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
        std::process::exit(1);
    }
}

fn run_benchmarks(args: &[String]) {
    let bench_type = args.get(0).map(|s| s.as_str()).unwrap_or("all");
    
    println!("╔═══════════════════════════════════════════════════════════════╗");
    println!("║           RosettaVM CPU vs GPU Benchmark                      ║");
    println!("╠═══════════════════════════════════════════════════════════════╣");
    
    match bench_type {
        "sum" => bench_sum(),
        "tree" => bench_tree(),
        "all" | _ => {
            bench_sum();
            println!("╠───────────────────────────────────────────────────────────────╣");
            bench_tree();
        }
    }
    
    println!("╚═══════════════════════════════════════════════════════════════╝");
}

fn bench_sum() {
    let n: u64 = 1_000_000;
    let expected: i64 = (n * (n + 1) / 2) as i64;
    
    println!("║ Sum Benchmark: sum(1..{})                            ║", n);
    println!("║ Expected result: {}                               ║", expected);
    println!("╟───────────────────────────────────────────────────────────────╢");
    
    // CPU benchmark using RVM interpreter
    print!("║ CPU (RVM interpreter)... ");
    let cpu_start = std::time::Instant::now();
    
    // Run sum_benchmark.rvm
    let sum_code = r#"
fn sum_to_n(n) {
    push 0
    push 1
loop_start:
    dup
    load 0
    le
    jf loop_end
    swap
    over
    add
    swap
    push 1
    add
    jmp loop_start
loop_end:
    pop
    ret
}
fn main() {
    push 1000000
    call sum_to_n
    halt
}
"#;
    let mut store = store::Store::new();
    let cpu_result = match parse::parse_file(sum_code, &mut store) {
        Ok(main_hash) => {
            let mut machine = vm::VM::new(&store);
            match machine.run(main_hash) {
                Ok(v) => v.as_int().map(|i| i.to_string()).unwrap_or("error".to_string()),
                Err(e) => format!("error: {}", e),
            }
        }
        Err(e) => format!("parse error: {}", e),
    };
    let cpu_time = cpu_start.elapsed();
    println!("{:>10} in {:>8.3}s ║", cpu_result, cpu_time.as_secs_f64());
    
    // GPU benchmark
    print!("║ GPU (CUDA)...            ");
    match cuda::gpu_sum(n) {
        Ok((result, time)) => {
            println!("{:>10} in {:>8.3}s ║", result, time.as_secs_f64());
            
            let speedup = cpu_time.as_secs_f64() / time.as_secs_f64();
            println!("║ Speedup: {:>6.2}x                                            ║", speedup);
        }
        Err(e) => println!("error: {} ║", e),
    }
}

fn bench_tree() {
    let depth: u32 = 20;
    let expected: u64 = 1 << depth;
    
    println!("║ Tree Sum Benchmark: tree_sum(depth={})                       ║", depth);
    println!("║ Expected result: {} (2^{})                          ║", expected, depth);
    println!("╟───────────────────────────────────────────────────────────────╢");
    
    // CPU benchmark using RVM interpreter
    print!("║ CPU (RVM interpreter)... ");
    let cpu_start = std::time::Instant::now();
    
    let tree_code = r#"
fn tree(depth) {
    load 0
    push 0
    eq
    jf make_node
    push 1
    con Tree 0 1
    ret
make_node:
    load 0
    push 1
    sub
    call tree
    load 0
    push 1
    sub
    call tree
    con Tree 1 2
    ret
}
fn sum(t) {
    load 0
    testtag 0
    jf sum_node
    getfield 0
    ret
sum_node:
    dup
    getfield 0
    call sum
    swap
    getfield 1
    call sum
    add
    ret
}
fn main() {
    push 20
    call tree
    call sum
    halt
}
"#;
    let mut store = store::Store::new();
    let cpu_result = match parse::parse_file(tree_code, &mut store) {
        Ok(main_hash) => {
            let mut machine = vm::VM::new(&store);
            match machine.run(main_hash) {
                Ok(v) => v.as_int().map(|i| i.to_string()).unwrap_or("error".to_string()),
                Err(e) => format!("error: {}", e),
            }
        }
        Err(e) => format!("parse error: {}", e),
    };
    let cpu_time = cpu_start.elapsed();
    println!("{:>10} in {:>8.3}s ║", cpu_result, cpu_time.as_secs_f64());
    
    // GPU benchmark
    print!("║ GPU (CUDA)...            ");
    match cuda::gpu_tree_sum(depth) {
        Ok((result, time)) => {
            println!("{:>10} in {:>8.3}s ║", result, time.as_secs_f64());
            
            let speedup = cpu_time.as_secs_f64() / time.as_secs_f64();
            println!("║ Speedup: {:>6.2}x                                            ║", speedup);
        }
        Err(e) => println!("error: {} ║", e),
    }
}

/// Compile RVM bytecode to CUDA
fn compile_to_cuda(path: &str) {
    let source = fs::read_to_string(path).expect("Cannot read file");
    let mut store = store::Store::new();
    
    match parse::parse_file(&source, &mut store) {
        Ok(main_hash) => {
            let mut codegen = cuda_codegen::CudaCodegen::new();
            let cuda_code = codegen.compile(&store, main_hash);
            
            // Output filename: replace .rvm with .cu
            let out_path = path.replace(".rvm", ".cu");
            fs::write(&out_path, &cuda_code).expect("Cannot write CUDA file");
            
            println!("✓ Generated {}", out_path);
            println!();
            println!("To compile and run:");
            println!("  nvcc -o {} {}", out_path.replace(".cu", ""), out_path);
            println!("  ./{}", out_path.replace(".cu", ""));
        }
        Err(e) => eprintln!("Parse error: {}", e),
    }
}

/// Compile Phi expression to RVM bytecode
fn compile_phi_expr(expr: &str) {
    use phi_compiler::PhiCompiler;
    
    let mut compiler = PhiCompiler::new();
    
    // Parse as a simple expression (numbers, strings, lists for now)
    let phi_expr = parse_phi_expression(expr);
    
    let bytecode = compiler.compile_to_string(&phi_expr);
    
    println!("// Phi expression: {}", expr);
    println!("// Generated RVM bytecode:");
    println!();
    println!("fn main() {{");
    for instr in &bytecode {
        println!("    {}", instr);
    }
    println!("    halt");
    println!("}}");
}

/// Compile Phi file to RVM
fn compile_phi_file(path: &str) {
    use phi_compiler::PhiCompiler;
    
    let source = fs::read_to_string(path).expect("Cannot read file");
    let mut compiler = PhiCompiler::new();
    
    // Parse file (simplified: treat as single expression for now)
    let phi_expr = parse_phi_expression(&source);
    let bytecode = compiler.compile_to_string(&phi_expr);
    
    // Output filename: replace .phi with .rvm
    let out_path = path.replace(".phi", ".rvm");
    
    let mut output = String::new();
    output.push_str("// Generated by: rvm compile\n");
    output.push_str(&format!("// Source: {}\n", path));
    output.push_str("\n");
    output.push_str("fn main() {\n");
    for instr in &bytecode {
        output.push_str(&format!("    {}\n", instr));
    }
    output.push_str("    halt\n");
    output.push_str("}\n");
    
    fs::write(&out_path, &output).expect("Cannot write RVM file");
    println!("✓ Generated {}", out_path);
}

/// Parse a Phi expression (simplified)
fn parse_phi_expression(s: &str) -> phi_compiler::PhiExpr {
    use phi_compiler::PhiExpr;
    
    let s = s.trim();
    
    // Integer literal
    if let Ok(n) = s.parse::<i64>() {
        return PhiExpr::Int(n);
    }
    
    // Float literal
    if s.contains('.') {
        if let Ok(f) = s.parse::<f64>() {
            return PhiExpr::Float(f);
        }
    }
    
    // Boolean
    if s == "true" {
        return PhiExpr::Bool(true);
    }
    if s == "false" {
        return PhiExpr::Bool(false);
    }
    
    // String (quoted)
    if s.starts_with('"') && s.ends_with('"') {
        return PhiExpr::Str(s[1..s.len()-1].to_string());
    }
    
    // List [a, b, c]
    if s.starts_with('[') && s.ends_with(']') {
        let inner = &s[1..s.len()-1];
        if inner.is_empty() {
            return PhiExpr::List(vec![]);
        }
        let elems: Vec<_> = split_preserving_brackets(inner)
            .iter()
            .map(|e| parse_phi_expression(e.trim()))
            .collect();
        return PhiExpr::List(elems);
    }
    
    // Lambda: \x -> body or λx.body
    if s.starts_with('\\') || s.starts_with('λ') {
        let rest = if s.starts_with('\\') { &s[1..] } else { &s[2..] };
        if let Some(arrow_pos) = rest.find("->") {
            let param = rest[..arrow_pos].trim().to_string();
            let body = rest[arrow_pos+2..].trim();
            return PhiExpr::Lambda(param, Box::new(parse_phi_expression(body)));
        } else if let Some(dot_pos) = rest.find('.') {
            let param = rest[..dot_pos].trim().to_string();
            let body = rest[dot_pos+1..].trim();
            return PhiExpr::Lambda(param, Box::new(parse_phi_expression(body)));
        }
    }
    
    // Let binding: let x = e1 in e2
    if s.starts_with("let ") {
        if let Some(eq_pos) = s.find('=') {
            let name = s[4..eq_pos].trim().to_string();
            let rest = &s[eq_pos+1..];
            if let Some(in_pos) = rest.find(" in ") {
                let value = rest[..in_pos].trim();
                let body = rest[in_pos+4..].trim();
                return PhiExpr::Let(
                    name,
                    Box::new(parse_phi_expression(value)),
                    Box::new(parse_phi_expression(body)),
                );
            }
        }
    }
    
    // If expression: if cond then e1 else e2
    if s.starts_with("if ") {
        if let Some(then_pos) = s.find(" then ") {
            if let Some(else_pos) = s.find(" else ") {
                let cond = &s[3..then_pos].trim();
                let then_branch = &s[then_pos+6..else_pos].trim();
                let else_branch = &s[else_pos+6..].trim();
                return PhiExpr::If(
                    Box::new(parse_phi_expression(cond)),
                    Box::new(parse_phi_expression(then_branch)),
                    Box::new(parse_phi_expression(else_branch)),
                );
            }
        }
    }
    
    // Binary operators (very basic)
    for (op_str, op) in [
        (" + ", phi_compiler::BinOp::Add),
        (" - ", phi_compiler::BinOp::Sub),
        (" * ", phi_compiler::BinOp::Mul),
        (" / ", phi_compiler::BinOp::Div),
        (" == ", phi_compiler::BinOp::Eq),
        (" < ", phi_compiler::BinOp::Lt),
        (" > ", phi_compiler::BinOp::Gt),
    ] {
        if let Some(pos) = s.find(op_str) {
            let left = &s[..pos].trim();
            let right = &s[pos+op_str.len()..].trim();
            return PhiExpr::BinOp(
                op,
                Box::new(parse_phi_expression(left)),
                Box::new(parse_phi_expression(right)),
            );
        }
    }
    
    // Function application: f(x) or (f x)
    if s.ends_with(')') {
        if let Some(paren_pos) = s.find('(') {
            let func_name = &s[..paren_pos].trim();
            let args_str = &s[paren_pos+1..s.len()-1];
            if !func_name.is_empty() && !func_name.contains(' ') {
                let args: Vec<_> = split_preserving_brackets(args_str)
                    .iter()
                    .filter(|a| !a.trim().is_empty())
                    .map(|a| parse_phi_expression(a.trim()))
                    .collect();
                
                let mut result = PhiExpr::Var(func_name.to_string());
                for arg in args {
                    result = PhiExpr::App(Box::new(result), Box::new(arg));
                }
                return result;
            }
        }
    }
    
    // Variable
    if s.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return PhiExpr::Var(s.to_string());
    }
    
    // Fallback: treat as string
    PhiExpr::Str(s.to_string())
}

/// Split by comma, preserving brackets
fn split_preserving_brackets(s: &str) -> Vec<&str> {
    let mut result = vec![];
    let mut depth = 0;
    let mut start = 0;
    
    for (i, c) in s.char_indices() {
        match c {
            '[' | '(' | '{' => depth += 1,
            ']' | ')' | '}' => depth -= 1,
            ',' if depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    
    if start < s.len() {
        result.push(&s[start..]);
    }
    
    result
}