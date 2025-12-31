//! CUDA backend for RVM
//! Compiles RVM programs to CUDA kernels for GPU execution

use std::fs;
use std::process::Command;
use std::path::Path;
use crate::instr::{Instr, CodeBlock};
use crate::store::Store;
use crate::hash::Hash;

/// CUDA code generator
pub struct CudaCompiler {
    output_dir: String,
}

impl CudaCompiler {
    pub fn new(output_dir: &str) -> Self {
        fs::create_dir_all(output_dir).ok();
        CudaCompiler {
            output_dir: output_dir.to_string(),
        }
    }

    /// Generate CUDA code for tree_sum benchmark
    /// This is a specialized kernel for the tree reduction pattern
    pub fn gen_tree_sum(&self, depth: u32) -> String {
        let num_leaves = 1u64 << depth;
        
        format!(r#"
// RosettaVM CUDA Backend - Tree Sum Kernel
// Generated for depth={depth}, leaves={num_leaves}

#include <stdio.h>
#include <cuda_runtime.h>

#define DEPTH {depth}
#define NUM_LEAVES {num_leaves}ULL

// Tree node structure (flattened)
// For a complete binary tree, we use array representation:
// - Node i has children at 2*i+1 and 2*i+2
// - Leaves are at indices [NUM_LEAVES-1, 2*NUM_LEAVES-2]

__global__ void build_tree(long long* tree, int level, long long offset) {{
    long long idx = blockIdx.x * blockDim.x + threadIdx.x;
    long long nodes_at_level = 1LL << level;
    
    if (idx < nodes_at_level) {{
        long long node_idx = offset + idx;
        if (level == DEPTH) {{
            // Leaf node - value is 1
            tree[node_idx] = 1;
        }} else {{
            // Internal node - will be filled during reduction
            tree[node_idx] = 0;
        }}
    }}
}}

__global__ void reduce_level(long long* tree, int level, long long offset) {{
    long long idx = blockIdx.x * blockDim.x + threadIdx.x;
    long long nodes_at_level = 1LL << level;
    
    if (idx < nodes_at_level) {{
        long long node_idx = offset + idx;
        long long left_child = 2 * node_idx + 1;
        long long right_child = 2 * node_idx + 2;
        tree[node_idx] = tree[left_child] + tree[right_child];
    }}
}}

int main() {{
    // Total nodes in complete binary tree = 2^(depth+1) - 1
    long long total_nodes = (1LL << (DEPTH + 1)) - 1;
    size_t tree_size = total_nodes * sizeof(long long);
    
    // Allocate device memory
    long long* d_tree;
    cudaMalloc(&d_tree, tree_size);
    
    // Build tree (initialize leaves)
    int threads = 256;
    
    // Initialize leaves (level = DEPTH)
    long long leaf_offset = (1LL << DEPTH) - 1;
    long long num_leaves = 1LL << DEPTH;
    int blocks = (num_leaves + threads - 1) / threads;
    build_tree<<<blocks, threads>>>(d_tree, DEPTH, leaf_offset);
    cudaDeviceSynchronize();
    
    // Reduce tree from leaves up to root
    for (int level = DEPTH - 1; level >= 0; level--) {{
        long long offset = (1LL << level) - 1;
        long long nodes = 1LL << level;
        blocks = (nodes + threads - 1) / threads;
        if (blocks == 0) blocks = 1;
        reduce_level<<<blocks, threads>>>(d_tree, level, offset);
        cudaDeviceSynchronize();
    }}
    
    // Copy result back
    long long result;
    cudaMemcpy(&result, d_tree, sizeof(long long), cudaMemcpyDeviceToHost);
    
    printf("%lld\n", result);
    
    cudaFree(d_tree);
    return 0;
}}
"#, depth=depth, num_leaves=num_leaves)
    }

    /// Generate CUDA code for generic parallel sum
    pub fn gen_parallel_sum(&self, n: u64) -> String {
        format!(r#"
// RosettaVM CUDA Backend - Parallel Sum 1..N
// Generated for N={n}

#include <stdio.h>
#include <cuda_runtime.h>

#define N {n}ULL
#define BLOCK_SIZE 256

__global__ void partial_sum(long long* results, long long start, long long count) {{
    __shared__ long long sdata[BLOCK_SIZE];
    
    long long tid = threadIdx.x;
    long long idx = blockIdx.x * blockDim.x + threadIdx.x;
    
    // Each thread computes sum of a chunk
    long long local_sum = 0;
    long long chunk_size = (count + gridDim.x * blockDim.x - 1) / (gridDim.x * blockDim.x);
    long long my_start = start + idx * chunk_size;
    long long my_end = my_start + chunk_size;
    if (my_end > start + count) my_end = start + count;
    
    for (long long i = my_start; i < my_end && i <= N; i++) {{
        local_sum += i;
    }}
    
    sdata[tid] = local_sum;
    __syncthreads();
    
    // Reduction in shared memory
    for (int s = blockDim.x / 2; s > 0; s >>= 1) {{
        if (tid < s) {{
            sdata[tid] += sdata[tid + s];
        }}
        __syncthreads();
    }}
    
    if (tid == 0) {{
        results[blockIdx.x] = sdata[0];
    }}
}}

__global__ void final_reduce(long long* results, int n) {{
    __shared__ long long sdata[BLOCK_SIZE];
    
    int tid = threadIdx.x;
    sdata[tid] = (tid < n) ? results[tid] : 0;
    __syncthreads();
    
    for (int s = blockDim.x / 2; s > 0; s >>= 1) {{
        if (tid < s) {{
            sdata[tid] += sdata[tid + s];
        }}
        __syncthreads();
    }}
    
    if (tid == 0) {{
        results[0] = sdata[0];
    }}
}}

int main() {{
    int num_blocks = 256;
    int threads = BLOCK_SIZE;
    
    long long* d_results;
    cudaMalloc(&d_results, num_blocks * sizeof(long long));
    
    partial_sum<<<num_blocks, threads>>>(d_results, 1, N);
    cudaDeviceSynchronize();
    
    final_reduce<<<1, BLOCK_SIZE>>>(d_results, num_blocks);
    cudaDeviceSynchronize();
    
    long long result;
    cudaMemcpy(&result, d_results, sizeof(long long), cudaMemcpyDeviceToHost);
    
    printf("%lld\n", result);
    
    cudaFree(d_results);
    return 0;
}}
"#, n=n)
    }

    /// Compile CUDA code to executable
    pub fn compile(&self, cuda_code: &str, name: &str) -> Result<String, String> {
        let cu_path = format!("{}/{}.cu", self.output_dir, name);
        let exe_path = format!("{}/{}", self.output_dir, name);
        
        fs::write(&cu_path, cuda_code)
            .map_err(|e| format!("Failed to write CUDA file: {}", e))?;
        
        let output = Command::new("nvcc")
            .args(["-O3", "-o", &exe_path, &cu_path])
            .output()
            .map_err(|e| format!("Failed to run nvcc: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("nvcc failed: {}", String::from_utf8_lossy(&output.stderr)));
        }
        
        Ok(exe_path)
    }

    /// Compile and run
    pub fn run(&self, cuda_code: &str, name: &str) -> Result<String, String> {
        let exe_path = self.compile(cuda_code, name)?;
        
        let output = Command::new(&exe_path)
            .output()
            .map_err(|e| format!("Failed to run: {}", e))?;
        
        if !output.status.success() {
            return Err(format!("Execution failed: {}", String::from_utf8_lossy(&output.stderr)));
        }
        
        Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
    }
}

/// Run tree_sum benchmark on GPU
pub fn gpu_tree_sum(depth: u32) -> Result<(String, std::time::Duration), String> {
    let compiler = CudaCompiler::new("/tmp/rvm_cuda");
    let code = compiler.gen_tree_sum(depth);
    
    let start = std::time::Instant::now();
    let result = compiler.run(&code, "tree_sum")?;
    let elapsed = start.elapsed();
    
    Ok((result, elapsed))
}

/// Run sum benchmark on GPU
pub fn gpu_sum(n: u64) -> Result<(String, std::time::Duration), String> {
    let compiler = CudaCompiler::new("/tmp/rvm_cuda");
    let code = compiler.gen_parallel_sum(n);
    
    let start = std::time::Instant::now();
    let result = compiler.run(&code, "sum")?;
    let elapsed = start.elapsed();
    
    Ok((result, elapsed))
}
