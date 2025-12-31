//! Generic CUDA code generator for RVM bytecode
//! Compiles arbitrary RVM programs to CUDA kernels

use std::collections::HashMap;
use crate::instr::{Instr, CodeBlock, Literal};
use crate::store::Store;
use crate::hash::Hash;

/// Compiles RVM bytecode to CUDA C code
pub struct CudaCodegen {
    /// Function definitions (hash -> generated code)
    functions: HashMap<Hash, String>,
    /// Function names (hash -> name)
    func_names: HashMap<Hash, String>,
    /// Function arities (hash -> arity)
    func_arities: HashMap<Hash, u8>,
    /// Entry point hash
    entry_hash: Option<Hash>,
    /// Global data
    globals: Vec<String>,
    /// String table
    strings: Vec<String>,
}

impl CudaCodegen {
    pub fn new() -> Self {
        CudaCodegen {
            functions: HashMap::new(),
            func_names: HashMap::new(),
            func_arities: HashMap::new(),
            entry_hash: None,
            globals: Vec::new(),
            strings: Vec::new(),
        }
    }

    /// Compile a complete RVM program to CUDA
    pub fn compile(&mut self, store: &Store, entry: Hash) -> String {
        self.entry_hash = Some(entry);
        
        // Collect all reachable functions
        self.collect_functions(store, entry);
        
        // Generate CUDA code
        self.generate_cuda()
    }

    fn collect_functions(&mut self, store: &Store, hash: Hash) {
        if self.functions.contains_key(&hash) {
            return;
        }

        if let Some(block) = store.get_code(&hash) {
            let name = block.name.as_ref()
                .map(|n| {
                    let sanitized = sanitize_name(n);
                    if sanitized == "main" { "rvm_main".to_string() } else { sanitized }
                })
                .unwrap_or_else(|| format!("fn_{}", block.hash.short()));
            
            self.func_names.insert(hash, name);
            self.func_arities.insert(hash, block.arity);

            // Recursively collect called functions first (so we have their names)
            for instr in &block.code {
                match instr {
                    Instr::Call(h) | Instr::TailCall(h) | Instr::CallN(h, _) => {
                        self.collect_functions(store, *h);
                    }
                    Instr::Closure(h, _) => {
                        self.collect_functions(store, *h);
                    }
                    _ => {}
                }
            }
            
            // Now compile this function (after we know all callee names)
            let code = self.compile_function(block);
            self.functions.insert(hash, code);
        }
    }

    fn compile_function(&mut self, block: &CodeBlock) -> String {
        let name = block.name.as_ref()
            .map(|n| sanitize_name(n))
            .unwrap_or_else(|| format!("fn_{}", block.hash.short()));
        
        // Rename "main" to avoid conflict with C main
        let func_name = if name == "main" { "rvm_main".to_string() } else { name };
        
        let mut code = String::new();
        code.push_str(&format!("__device__ Val {}(Val* stack, int* sp, Val* env) {{\n", func_name));
        
        // Local variables for stack manipulation
        code.push_str("    Val tmp, tmp2, tmp3;\n");
        code.push_str("    int cond;\n\n");

        // Compile each instruction
        for (pc, instr) in block.code.iter().enumerate() {
            code.push_str(&format!("L{}:\n", pc));
            code.push_str(&self.compile_instr(instr, pc, block.code.len()));
        }

        code.push_str("}\n\n");
        code
    }

    fn compile_instr(&mut self, instr: &Instr, pc: usize, total: usize) -> String {
        match instr {
            // Stack operations
            Instr::Push(lit) => {
                let val = self.literal_to_cuda(lit);
                format!("    stack[++(*sp)] = {};\n", val)
            }
            Instr::Pop => "    (*sp)--;\n".to_string(),
            Instr::Dup => "    stack[*sp + 1] = stack[*sp]; (*sp)++;\n".to_string(),
            Instr::Swap => {
                "    tmp = stack[*sp]; stack[*sp] = stack[*sp-1]; stack[*sp-1] = tmp;\n".to_string()
            }
            Instr::Over => "    stack[*sp + 1] = stack[*sp - 1]; (*sp)++;\n".to_string(),

            // Environment
            Instr::Load(idx) => format!("    stack[++(*sp)] = env[{}];\n", idx),
            Instr::Store(idx) => format!("    env[{}] = stack[(*sp)--];\n", idx),

            // Arithmetic
            Instr::Add => "    stack[*sp-1].i += stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Sub => "    stack[*sp-1].i -= stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Mul => "    stack[*sp-1].i *= stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Div => "    stack[*sp-1].i /= stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Mod => "    stack[*sp-1].i %= stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Neg => "    stack[*sp].i = -stack[*sp].i;\n".to_string(),

            // Comparison
            Instr::Eq => "    stack[*sp-1].i = (stack[*sp-1].i == stack[*sp].i); (*sp)--;\n".to_string(),
            Instr::Ne => "    stack[*sp-1].i = (stack[*sp-1].i != stack[*sp].i); (*sp)--;\n".to_string(),
            Instr::Lt => "    stack[*sp-1].i = (stack[*sp-1].i < stack[*sp].i); (*sp)--;\n".to_string(),
            Instr::Le => "    stack[*sp-1].i = (stack[*sp-1].i <= stack[*sp].i); (*sp)--;\n".to_string(),
            Instr::Gt => "    stack[*sp-1].i = (stack[*sp-1].i > stack[*sp].i); (*sp)--;\n".to_string(),
            Instr::Ge => "    stack[*sp-1].i = (stack[*sp-1].i >= stack[*sp].i); (*sp)--;\n".to_string(),

            // Boolean
            Instr::Not => "    stack[*sp].i = !stack[*sp].i;\n".to_string(),
            Instr::And => "    stack[*sp-1].i = stack[*sp-1].i && stack[*sp].i; (*sp)--;\n".to_string(),
            Instr::Or => "    stack[*sp-1].i = stack[*sp-1].i || stack[*sp].i; (*sp)--;\n".to_string(),

            // Control flow
            Instr::Jump(offset) => {
                let target = (pc as i32 + offset) as usize;
                format!("    goto L{};\n", target)
            }
            Instr::JumpIf(offset) => {
                let target = (pc as i32 + offset) as usize;
                format!("    cond = stack[(*sp)--].i; if (cond) goto L{};\n", target)
            }
            Instr::JumpIfNot(offset) => {
                let target = (pc as i32 + offset) as usize;
                format!("    cond = stack[(*sp)--].i; if (!cond) goto L{};\n", target)
            }
            Instr::Return => "    return stack[*sp];\n".to_string(),
            Instr::Halt => "    return stack[*sp];\n".to_string(),
            
            // Function calls - pop args from stack into env, then call
            Instr::Call(h) => {
                let target_name = self.func_names.get(h)
                    .map(|s| s.clone())
                    .unwrap_or_else(|| format!("fn_{}", h.short()));
                let arity = self.func_arities.get(h).copied().unwrap_or(0);
                
                let mut code = String::new();
                // Pop arity args from stack into env (in reverse order)
                for i in (0..arity).rev() {
                    code.push_str(&format!("    env[{}] = stack[(*sp)--];\n", i));
                }
                code.push_str(&format!("    stack[++(*sp)] = {}(stack, sp, env);\n", target_name));
                code
            }
            Instr::TailCall(h) => {
                let target_name = self.func_names.get(h)
                    .map(|s| s.clone())
                    .unwrap_or_else(|| format!("fn_{}", h.short()));
                let arity = self.func_arities.get(h).copied().unwrap_or(0);
                
                let mut code = String::new();
                for i in (0..arity).rev() {
                    code.push_str(&format!("    env[{}] = stack[(*sp)--];\n", i));
                }
                code.push_str(&format!("    return {}(stack, sp, env);\n", target_name));
                code
            }

            // For now, other instructions are no-ops in CUDA
            _ => format!("    // TODO: {:?}\n", instr),
        }
    }

    fn literal_to_cuda(&mut self, lit: &Literal) -> String {
        match lit {
            Literal::Int(n) => format!("make_int({}LL)", n),
            Literal::Bool(b) => format!("make_int({})", if *b { 1 } else { 0 }),
            Literal::Str(s) => {
                let idx = self.strings.len();
                self.strings.push(s.clone());
                format!("make_str({})", idx)
            }
            Literal::Unit => "make_int(0)".to_string(),
            Literal::Nil => "make_nil()".to_string(),
            _ => "make_int(0)".to_string(),
        }
    }

    /// Generate CUDA C code from compiled functions
    pub fn generate_cuda(&self) -> String {
        let mut code = String::new();

        // Header
        code.push_str(r#"
// RosettaVM CUDA Backend - Auto-generated
#include <stdio.h>
#include <cuda_runtime.h>

// Value representation
typedef union {
    long long i;
    double f;
    void* p;
} Val;

__device__ Val make_int(long long n) { Val v; v.i = n; return v; }
__device__ Val make_nil() { Val v; v.i = 0; return v; }
__device__ Val make_str(int idx) { Val v; v.i = idx; return v; }

#define STACK_SIZE 4096
#define ENV_SIZE 256

"#);

        // Function declarations
        for (hash, _) in &self.functions {
            let name = self.func_names.get(hash).map(|s| s.as_str()).unwrap_or("unknown");
            code.push_str(&format!("__device__ Val {}(Val* stack, int* sp, Val* env);\n", name));
        }
        code.push_str("\n");

        // Function definitions
        for (_, func_code) in &self.functions {
            code.push_str(func_code);
        }

        // Kernel wrapper
        code.push_str(r#"
__global__ void rvm_kernel(Val* results, Val* args, int n_tasks) {
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= n_tasks) return;

    Val stack[STACK_SIZE];
    Val env[ENV_SIZE];
    int sp = -1;

    // Load argument
    stack[++sp] = args[tid];

    // Call entry point
"#);

        // Call the entry point function
        if let Some(entry) = self.entry_hash {
            let name = self.func_names.get(&entry).map(|s| s.as_str()).unwrap_or("rvm_main");
            code.push_str(&format!("    results[tid] = {}(stack, &sp, env);\n", name));
        }

        code.push_str("}\n\n");

        // Host code
        code.push_str(r#"
int main(int argc, char** argv) {
    int n_tasks = 1;
    if (argc > 1) n_tasks = atoi(argv[1]);

    Val* d_results;
    Val* d_args;
    cudaMalloc(&d_results, n_tasks * sizeof(Val));
    cudaMalloc(&d_args, n_tasks * sizeof(Val));

    // Initialize args (task indices)
    Val* h_args = (Val*)malloc(n_tasks * sizeof(Val));
    for (int i = 0; i < n_tasks; i++) {
        h_args[i].i = i;
    }
    cudaMemcpy(d_args, h_args, n_tasks * sizeof(Val), cudaMemcpyHostToDevice);

    // Launch kernel
    int threads = 256;
    int blocks = (n_tasks + threads - 1) / threads;
    rvm_kernel<<<blocks, threads>>>(d_results, d_args, n_tasks);
    cudaDeviceSynchronize();

    // Get results
    Val* h_results = (Val*)malloc(n_tasks * sizeof(Val));
    cudaMemcpy(h_results, d_results, n_tasks * sizeof(Val), cudaMemcpyDeviceToHost);

    // Sum results (for reduction benchmarks)
    long long total = 0;
    for (int i = 0; i < n_tasks; i++) {
        total += h_results[i].i;
    }
    printf("%lld\n", total);

    free(h_args);
    free(h_results);
    cudaFree(d_results);
    cudaFree(d_args);
    return 0;
}
"#);

        code
    }
}

fn sanitize_name(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c } else { '_' })
        .collect()
}

/// Compile RVM bytecode to CUDA and return the source
pub fn compile_to_cuda(store: &Store, entry: Hash) -> String {
    let mut codegen = CudaCodegen::new();
    codegen.compile(store, entry)
}
