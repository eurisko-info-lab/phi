//! Parallel execution for RVM using Rayon
//! Provides multi-threaded CPU execution for tree-parallel operations

use rayon::prelude::*;
use std::time::Instant;

/// Parallel tree sum - builds and sums a tree in parallel
pub fn parallel_tree_sum(depth: u32) -> i64 {
    if depth == 0 {
        return 1;
    }
    
    // Build tree levels in parallel
    let num_leaves = 1u64 << depth;
    
    // Use parallel reduction
    (0..num_leaves)
        .into_par_iter()
        .map(|_| 1i64)
        .sum()
}

/// Parallel sum 1..n
pub fn parallel_sum(n: u64) -> i64 {
    (1..=n)
        .into_par_iter()
        .map(|x| x as i64)
        .sum()
}

/// Parallel map-reduce over a range
pub fn parallel_map_reduce<F, G, T>(start: u64, end: u64, map: F, reduce: G) -> T
where
    F: Fn(u64) -> T + Sync + Send,
    G: Fn(T, T) -> T + Sync + Send,
    T: Send + Default,
{
    (start..end)
        .into_par_iter()
        .map(map)
        .reduce(T::default, reduce)
}

/// Parallel fibonacci (inefficient but demonstrates parallelism)
pub fn parallel_fib(n: u32) -> u64 {
    if n <= 1 {
        return n as u64;
    }
    
    let (a, b) = rayon::join(
        || parallel_fib(n - 1),
        || parallel_fib(n - 2),
    );
    
    a + b
}

/// Parallel quicksort
pub fn parallel_quicksort<T: Ord + Send>(arr: &mut [T]) {
    if arr.len() <= 1 {
        return;
    }
    
    if arr.len() < 32 {
        // Use sequential sort for small arrays
        arr.sort();
        return;
    }
    
    let pivot_idx = partition(arr);
    let (left, right) = arr.split_at_mut(pivot_idx);
    
    rayon::join(
        || parallel_quicksort(left),
        || parallel_quicksort(&mut right[1..]),
    );
}

fn partition<T: Ord>(arr: &mut [T]) -> usize {
    let len = arr.len();
    let pivot_idx = len / 2;
    arr.swap(pivot_idx, len - 1);
    
    let mut i = 0;
    for j in 0..len - 1 {
        if arr[j] <= arr[len - 1] {
            arr.swap(i, j);
            i += 1;
        }
    }
    arr.swap(i, len - 1);
    i
}

/// Sequential fibonacci for comparison
fn sequential_fib(n: u32) -> u64 {
    if n <= 1 {
        return n as u64;
    }
    sequential_fib(n - 1) + sequential_fib(n - 2)
}

/// Sequential sum for comparison
fn sequential_sum(n: u64) -> i64 {
    (1..=n).map(|x| x as i64).sum()
}

/// Sequential tree sum for comparison
fn sequential_tree_sum(depth: u32) -> i64 {
    if depth == 0 {
        return 1;
    }
    let num_leaves = 1u64 << depth;
    (0..num_leaves).map(|_| 1i64).sum()
}

/// Benchmark runner for parallel operations
pub struct ParallelBench;

impl ParallelBench {
    pub fn run_all() {
        println!("╔═══════════════════════════════════════════════════════════════╗");
        println!("║           RosettaVM Parallel CPU Benchmarks (Rayon)           ║");
        println!("╠═══════════════════════════════════════════════════════════════╣");
        
        // Tree sum
        let depth = 24;
        println!("║ Tree Sum (depth={})                                           ║", depth);
        
        let seq_start = Instant::now();
        let seq_result = sequential_tree_sum(depth);
        let seq_time = seq_start.elapsed();
        println!("║ Sequential:      {:>12} in {:>8.3}s               ║", seq_result, seq_time.as_secs_f64());
        
        let par_start = Instant::now();
        let par_result = parallel_tree_sum(depth);
        let par_time = par_start.elapsed();
        println!("║ Parallel:        {:>12} in {:>8.3}s               ║", par_result, par_time.as_secs_f64());
        
        let speedup = seq_time.as_secs_f64() / par_time.as_secs_f64();
        println!("║ Speedup:        {:>6.2}x                                       ║", speedup);
        
        println!("╠───────────────────────────────────────────────────────────────╣");
        
        // Sum 
        let n = 100_000_000u64;
        println!("║ Sum (1..{})                                       ║", n);
        
        let seq_start = Instant::now();
        let seq_result = sequential_sum(n);
        let seq_time = seq_start.elapsed();
        println!("║ Sequential: {:>18} in {:>8.3}s          ║", seq_result, seq_time.as_secs_f64());
        
        let par_start = Instant::now();
        let par_result = parallel_sum(n);
        let par_time = par_start.elapsed();
        println!("║ Parallel:   {:>18} in {:>8.3}s          ║", par_result, par_time.as_secs_f64());
        
        let speedup = seq_time.as_secs_f64() / par_time.as_secs_f64();
        println!("║ Speedup:        {:>6.2}x                                       ║", speedup);
        
        println!("╠───────────────────────────────────────────────────────────────╣");
        
        // Fibonacci
        let fib_n = 35;
        println!("║ Fibonacci ({})                                                 ║", fib_n);
        
        let seq_start = Instant::now();
        let seq_result = sequential_fib(fib_n);
        let seq_time = seq_start.elapsed();
        println!("║ Sequential:      {:>12} in {:>8.3}s               ║", seq_result, seq_time.as_secs_f64());
        
        let par_start = Instant::now();
        let par_result = parallel_fib(fib_n);
        let par_time = par_start.elapsed();
        println!("║ Parallel:        {:>12} in {:>8.3}s               ║", par_result, par_time.as_secs_f64());
        
        let speedup = seq_time.as_secs_f64() / par_time.as_secs_f64();
        println!("║ Speedup:        {:>6.2}x                                       ║", speedup);
        
        println!("╠───────────────────────────────────────────────────────────────╣");
        
        // Quicksort
        let size = 10_000_000;
        println!("║ Quicksort ({} elements)                               ║", size);
        
        let mut seq_arr: Vec<i64> = (0..size).map(|i| (i * 7 + 13) % 1000000).collect();
        let seq_start = Instant::now();
        seq_arr.sort();
        let seq_time = seq_start.elapsed();
        println!("║ Sequential (std):           in {:>8.3}s               ║", seq_time.as_secs_f64());
        
        let mut par_arr: Vec<i64> = (0..size).map(|i| (i * 7 + 13) % 1000000).collect();
        let par_start = Instant::now();
        parallel_quicksort(&mut par_arr);
        let par_time = par_start.elapsed();
        println!("║ Parallel:                   in {:>8.3}s               ║", par_time.as_secs_f64());
        
        let speedup = seq_time.as_secs_f64() / par_time.as_secs_f64();
        println!("║ Speedup:        {:>6.2}x                                       ║", speedup);
        
        println!("╚═══════════════════════════════════════════════════════════════╝");
        
        println!("\n💡 Rayon uses work-stealing for automatic load balancing.");
        println!("   Number of threads: {}", rayon::current_num_threads());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parallel_sum() {
        assert_eq!(parallel_sum(100), 5050);
    }

    #[test]
    fn test_parallel_tree_sum() {
        assert_eq!(parallel_tree_sum(4), 16);
        assert_eq!(parallel_tree_sum(10), 1024);
    }

    #[test]
    fn test_parallel_fib() {
        assert_eq!(parallel_fib(10), 55);
        assert_eq!(parallel_fib(20), 6765);
    }

    #[test]
    fn test_parallel_quicksort() {
        let mut arr = vec![5, 2, 8, 1, 9, 3, 7, 4, 6];
        parallel_quicksort(&mut arr);
        assert_eq!(arr, vec![1, 2, 3, 4, 5, 6, 7, 8, 9]);
    }
}
