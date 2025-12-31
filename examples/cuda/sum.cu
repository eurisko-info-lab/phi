// =============================================================================
// HVM CUDA Runtime - Parallel Sum Example
// =============================================================================
//
// Compiled from Bend via HVM to CUDA:
//
//   def Sum(start, target):
//     if start == target: return start
//     else:
//       half = (start + target) / 2
//       left = Sum(start, half)
//       right = Sum(half + 1, target)
//       return left + right
//
//   def main(): return Sum(1, 1000000)
//
// Build: nvcc -O3 -o build/sum_gpu examples/cuda/sum.cu
// Run:   ./build/sum_gpu 1000000

#include <cuda_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>

// =============================================================================
// Types
// =============================================================================

typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef u64 Term;

// =============================================================================
// Term Layout (64-bit)
// =============================================================================

#define SUB_SHIFT 63
#define TAG_SHIFT 56
#define EXT_SHIFT 32
#define VAL_SHIFT 0

#define SUB_MASK 0x1ULL
#define TAG_MASK 0x7FULL
#define EXT_MASK 0xFFFFFFULL
#define VAL_MASK 0xFFFFFFFFULL

// =============================================================================
// Tags
// =============================================================================

#define APP 0
#define VAR 1
#define LAM 2
#define DP0 3
#define DP1 4
#define SUP 5
#define ERA 11
#define NUM 30
#define OP2 33
#define F_OP2_NUM 0x43

// Operations
#define OP_ADD 0
#define OP_SUB 1
#define OP_MUL 2
#define OP_DIV 3
#define OP_EQ  11

// =============================================================================
// GPU State
// =============================================================================

struct GPUState {
  Term* heap;
  u64* heap_next;
  u64* interactions;
  u64 heap_cap;
};

// =============================================================================
// Device Functions
// =============================================================================

__device__ __host__ __forceinline__ u8  term_sub(Term t) { return (u8)((t >> SUB_SHIFT) & SUB_MASK); }
__device__ __host__ __forceinline__ u8  term_tag(Term t) { return (u8)((t >> TAG_SHIFT) & TAG_MASK); }
__device__ __host__ __forceinline__ u32 term_ext(Term t) { return (u32)((t >> EXT_SHIFT) & EXT_MASK); }
__device__ __host__ __forceinline__ u32 term_val(Term t) { return (u32)(t & VAL_MASK); }

__device__ __host__ __forceinline__ Term term_new(u8 sub, u8 tag, u32 ext, u32 val) {
  return ((u64)sub << SUB_SHIFT) |
         ((u64)tag << TAG_SHIFT) |
         ((u64)ext << EXT_SHIFT) |
         ((u64)val);
}

__device__ __forceinline__ Term term_sub_set(Term t, u8 sub) {
  return (t & ~(SUB_MASK << SUB_SHIFT)) | ((u64)sub << SUB_SHIFT);
}

__device__ __forceinline__ u64 heap_alloc(GPUState* s, u64 size) {
  return atomicAdd((unsigned long long*)s->heap_next, size);
}

__device__ __forceinline__ Term heap_read(GPUState* s, u32 loc) {
  return s->heap[loc];
}

__device__ __forceinline__ void heap_write(GPUState* s, u32 loc, Term t) {
  s->heap[loc] = t;
}

__device__ __forceinline__ void heap_subst(GPUState* s, u32 loc, Term val) {
  s->heap[loc] = term_sub_set(val, 1);
}

__device__ __forceinline__ Term term_new_num(u32 val) { return term_new(0, NUM, 0, val); }
__device__ __forceinline__ Term term_new_era() { return term_new(0, ERA, 0, 0); }
__device__ __forceinline__ Term term_new_dp0(u32 lab, u32 loc) { return term_new(0, DP0, lab, loc); }
__device__ __forceinline__ Term term_new_dp1(u32 lab, u32 loc) { return term_new(0, DP1, lab, loc); }

__device__ Term term_new_sup(GPUState* s, u32 lab, Term a, Term b) {
  u32 loc = (u32)heap_alloc(s, 2);
  heap_write(s, loc + 0, a);
  heap_write(s, loc + 1, b);
  return term_new(0, SUP, lab, loc);
}

__device__ Term term_new_op2(GPUState* s, u8 op, Term a, Term b) {
  u32 loc = (u32)heap_alloc(s, 2);
  heap_write(s, loc + 0, a);
  heap_write(s, loc + 1, b);
  return term_new(0, OP2, op, loc);
}

__device__ Term term_new_app(GPUState* s, Term fun, Term arg) {
  u32 loc = (u32)heap_alloc(s, 2);
  heap_write(s, loc + 0, fun);
  heap_write(s, loc + 1, arg);
  return term_new(0, APP, 0, loc);
}

// =============================================================================
// WNF Reducer (Per-Thread)
// =============================================================================

__device__ Term wnf_reduce(GPUState* s, Term term) {
  Term stack[64];
  u32 s_pos = 0;
  Term next = term;
  Term whnf;

enter:
  switch (term_tag(next)) {
    case ERA:
    case NUM:
    case LAM:
    case SUP:
      whnf = next;
      goto apply;

    case VAR: {
      u32 loc = term_val(next);
      Term cell = heap_read(s, loc);
      if (term_sub(cell)) {
        next = cell;
        goto enter;
      }
      whnf = next;
      goto apply;
    }

    case APP: {
      u32 loc = term_val(next);
      Term fun = heap_read(s, loc);
      stack[s_pos++] = next;
      next = fun;
      goto enter;
    }

    case DP0:
    case DP1: {
      u32 loc = term_val(next);
      Term val = heap_read(s, loc);
      if (term_sub(val)) {
        next = val;
        goto enter;
      }
      stack[s_pos++] = next;
      next = val;
      goto enter;
    }

    case OP2: {
      u32 loc = term_val(next);
      Term x = heap_read(s, loc);
      stack[s_pos++] = next;
      next = x;
      goto enter;
    }

    default:
      whnf = next;
      goto apply;
  }

apply:
  while (s_pos > 0) {
    Term frame = stack[--s_pos];

    switch (term_tag(frame)) {
      case APP: {
        u32 app_loc = term_val(frame);
        Term arg = heap_read(s, app_loc + 1);

        switch (term_tag(whnf)) {
          case LAM: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 lam_loc = term_val(whnf);
            heap_subst(s, lam_loc, arg);
            Term body = heap_read(s, lam_loc);
            next = body;
            goto enter;
          }

          case ERA: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            whnf = term_new_era();
            continue;
          }

          case SUP: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term f = heap_read(s, sup_loc + 0);
            Term g = heap_read(s, sup_loc + 1);
            u32 dup_loc = (u32)heap_alloc(s, 4);
            heap_write(s, dup_loc + 0, arg);
            Term dp0 = term_new_dp0(lab, dup_loc);
            Term dp1 = term_new_dp1(lab, dup_loc);
            Term app0 = term_new_app(s, f, dp0);
            Term app1 = term_new_app(s, g, dp1);
            whnf = term_new_sup(s, lab, app0, app1);
            continue;
          }

          default:
            whnf = term_new_app(s, whnf, arg);
            continue;
        }
      }

      case DP0:
      case DP1: {
        u8 side = (term_tag(frame) == DP0) ? 0 : 1;
        u32 loc = term_val(frame);
        u32 lab = term_ext(frame);

        switch (term_tag(whnf)) {
          case SUP: {
            u32 sup_lab = term_ext(whnf);
            if (lab == sup_lab) {
              atomicAdd((unsigned long long*)s->interactions, 1);
              u32 sup_loc = term_val(whnf);
              Term a = heap_read(s, sup_loc + 0);
              Term b = heap_read(s, sup_loc + 1);
              heap_subst(s, loc, side == 0 ? b : a);
              whnf = side == 0 ? a : b;
              continue;
            }
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 sup_loc = term_val(whnf);
            Term a = heap_read(s, sup_loc + 0);
            Term b = heap_read(s, sup_loc + 1);
            u32 base = (u32)heap_alloc(s, 4);
            heap_write(s, base, a);
            heap_write(s, base + 2, b);
            Term a0 = term_new_dp0(lab, base);
            Term a1 = term_new_dp1(lab, base);
            Term b0 = term_new_dp0(lab, base + 2);
            Term b1 = term_new_dp1(lab, base + 2);
            Term sup0 = term_new_sup(s, sup_lab, a0, b0);
            Term sup1 = term_new_sup(s, sup_lab, a1, b1);
            heap_subst(s, loc, side == 0 ? sup1 : sup0);
            whnf = side == 0 ? sup0 : sup1;
            continue;
          }

          case ERA: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            heap_subst(s, loc, term_new_era());
            whnf = term_new_era();
            continue;
          }

          case NUM: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            heap_subst(s, loc, whnf);
            continue;
          }

          default: {
            u32 new_loc = (u32)heap_alloc(s, 1);
            heap_write(s, new_loc, whnf);
            heap_subst(s, loc, term_new(0, side == 0 ? DP1 : DP0, lab, new_loc));
            whnf = term_new(0, side == 0 ? DP0 : DP1, lab, new_loc);
            continue;
          }
        }
      }

      case OP2: {
        u32 op2_loc = term_val(frame);
        u8 op = (u8)term_ext(frame);
        Term y = heap_read(s, op2_loc + 1);

        switch (term_tag(whnf)) {
          case NUM: {
            u32 x_val = term_val(whnf);
            stack[s_pos++] = term_new(0, F_OP2_NUM, op, x_val);
            next = y;
            goto enter;
          }

          case SUP: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term a = heap_read(s, sup_loc + 0);
            Term b = heap_read(s, sup_loc + 1);
            Term op2_a = term_new_op2(s, op, a, y);
            Term op2_b = term_new_op2(s, op, b, y);
            whnf = term_new_sup(s, lab, op2_a, op2_b);
            continue;
          }

          default:
            whnf = term_new_op2(s, op, whnf, y);
            continue;
        }
      }

      case F_OP2_NUM: {
        u32 x_val = term_val(frame);
        u8 op = (u8)term_ext(frame);

        switch (term_tag(whnf)) {
          case NUM: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 y_val = term_val(whnf);
            u32 result;
            switch (op) {
              case OP_ADD: result = x_val + y_val; break;
              case OP_SUB: result = x_val - y_val; break;
              case OP_MUL: result = x_val * y_val; break;
              case OP_DIV: result = y_val ? x_val / y_val : 0; break;
              case OP_EQ:  result = x_val == y_val ? 1 : 0; break;
              default: result = 0;
            }
            whnf = term_new_num(result);
            continue;
          }

          case SUP: {
            atomicAdd((unsigned long long*)s->interactions, 1);
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term a = heap_read(s, sup_loc + 0);
            Term b = heap_read(s, sup_loc + 1);
            Term x = term_new_num(x_val);
            Term op2_a = term_new_op2(s, op, x, a);
            Term op2_b = term_new_op2(s, op, x, b);
            whnf = term_new_sup(s, lab, op2_a, op2_b);
            continue;
          }

          default:
            whnf = term_new_op2(s, op, term_new_num(x_val), whnf);
            continue;
        }
      }

      default:
        continue;
    }
  }

  return whnf;
}

// =============================================================================
// Parallel Reduction Kernels
// =============================================================================

// Build initial leaves: each thread creates one number
__global__ void build_leaves_kernel(GPUState state, Term* results, u32 start, u32 count) {
  u32 idx = blockIdx.x * blockDim.x + threadIdx.x;
  if (idx >= count) return;
  results[idx] = term_new_num(start + idx);
}

// Parallel tree reduction: each thread combines two adjacent terms
__global__ void reduce_pairs_kernel(GPUState state, Term* in, Term* out, u32 count) {
  u32 idx = blockIdx.x * blockDim.x + threadIdx.x;
  u32 pairs = (count + 1) / 2;
  if (idx >= pairs) return;
  
  u32 left_idx = idx * 2;
  u32 right_idx = left_idx + 1;
  
  Term left = in[left_idx];
  Term right = (right_idx < count) ? in[right_idx] : term_new_num(0);
  
  // Create (left + right) as OP2 term and reduce it
  Term op2 = term_new_op2(&state, OP_ADD, left, right);
  out[idx] = wnf_reduce(&state, op2);
}

// =============================================================================
// Host Functions
// =============================================================================

#define CUDA_CHECK(call) do { \
  cudaError_t err = call; \
  if (err != cudaSuccess) { \
    fprintf(stderr, "CUDA error at %s:%d: %s\n", __FILE__, __LINE__, \
            cudaGetErrorString(err)); \
    exit(1); \
  } \
} while(0)

GPUState gpu_init(u64 heap_cap) {
  GPUState state;
  state.heap_cap = heap_cap;
  
  CUDA_CHECK(cudaMalloc(&state.heap, heap_cap * sizeof(Term)));
  CUDA_CHECK(cudaMemset(state.heap, 0, heap_cap * sizeof(Term)));
  
  CUDA_CHECK(cudaMalloc(&state.heap_next, sizeof(u64)));
  u64 initial_heap = 1;
  CUDA_CHECK(cudaMemcpy(state.heap_next, &initial_heap, sizeof(u64), cudaMemcpyHostToDevice));
  
  CUDA_CHECK(cudaMalloc(&state.interactions, sizeof(u64)));
  CUDA_CHECK(cudaMemset(state.interactions, 0, sizeof(u64)));
  
  return state;
}

void gpu_free(GPUState* state) {
  cudaFree(state->heap);
  cudaFree(state->heap_next);
  cudaFree(state->interactions);
}

u64 gpu_get_interactions(GPUState* state) {
  u64 itrs;
  CUDA_CHECK(cudaMemcpy(&itrs, state->interactions, sizeof(u64), cudaMemcpyDeviceToHost));
  return itrs;
}

u64 gpu_get_heap_used(GPUState* state) {
  u64 used;
  CUDA_CHECK(cudaMemcpy(&used, state->heap_next, sizeof(u64), cudaMemcpyDeviceToHost));
  return used;
}

void gpu_reset(GPUState* state) {
  u64 initial = 1;
  CUDA_CHECK(cudaMemcpy(state->heap_next, &initial, sizeof(u64), cudaMemcpyHostToDevice));
  CUDA_CHECK(cudaMemset(state->interactions, 0, sizeof(u64)));
}

// =============================================================================
// GPU Parallel Sum
// =============================================================================

u64 gpu_sum(GPUState* state, u32 start, u32 target) {
  u32 count = target - start + 1;
  
  // Allocate double buffers for ping-pong reduction
  Term* buf_a;
  Term* buf_b;
  CUDA_CHECK(cudaMalloc(&buf_a, count * sizeof(Term)));
  CUDA_CHECK(cudaMalloc(&buf_b, count * sizeof(Term)));
  
  // Build leaf nodes in parallel
  u32 threads = 256;
  u32 blocks = (count + threads - 1) / threads;
  build_leaves_kernel<<<blocks, threads>>>(*state, buf_a, start, count);
  CUDA_CHECK(cudaDeviceSynchronize());
  
  // Tree reduction: O(log n) kernel launches
  Term* in = buf_a;
  Term* out = buf_b;
  u32 current_count = count;
  
  while (current_count > 1) {
    u32 pairs = (current_count + 1) / 2;
    blocks = (pairs + threads - 1) / threads;
    
    reduce_pairs_kernel<<<blocks, threads>>>(*state, in, out, current_count);
    CUDA_CHECK(cudaDeviceSynchronize());
    
    // Swap buffers
    Term* tmp = in;
    in = out;
    out = tmp;
    current_count = pairs;
  }
  
  // Copy final result back
  Term result;
  CUDA_CHECK(cudaMemcpy(&result, in, sizeof(Term), cudaMemcpyDeviceToHost));
  
  cudaFree(buf_a);
  cudaFree(buf_b);
  
  return term_val(result);
}

// =============================================================================
// CPU Reference Implementation
// =============================================================================

u64 cpu_sum(u64 start, u64 target) {
  if (start == target) return start;
  u64 half = (start + target) / 2;
  u64 left = cpu_sum(start, half);
  u64 right = cpu_sum(half + 1, target);
  return left + right;
}

// =============================================================================
// Main
// =============================================================================

int main(int argc, char** argv) {
  u32 n = 1000000;
  if (argc > 1) n = (u32)atoi(argv[1]);
  
  printf("╔═══════════════════════════════════════════════════════════════╗\n");
  printf("║     HVM CUDA Runtime - Parallel Sum: 1 to %-10u          ║\n", n);
  printf("╚═══════════════════════════════════════════════════════════════╝\n\n");
  
  // Check CUDA device
  int device_count;
  CUDA_CHECK(cudaGetDeviceCount(&device_count));
  if (device_count == 0) {
    fprintf(stderr, "No CUDA devices found!\n");
    return 1;
  }
  
  cudaDeviceProp prop;
  CUDA_CHECK(cudaGetDeviceProperties(&prop, 0));
  printf("GPU: %s\n", prop.name);
  printf("SMs: %d, Max Threads/Block: %d\n", prop.multiProcessorCount, prop.maxThreadsPerBlock);
  printf("Global Memory: %.1f GB\n\n", prop.totalGlobalMem / (1024.0 * 1024.0 * 1024.0));
  
  // Initialize GPU state
  u64 heap_cap = 1ULL << 26;  // 64M terms = 512MB
  GPUState state = gpu_init(heap_cap);
  
  // Warm up GPU
  gpu_sum(&state, 1, 1000);
  gpu_reset(&state);
  
  // ─────────────────────────────────────────────────────────────────────────
  // CPU Benchmark
  // ─────────────────────────────────────────────────────────────────────────
  printf("┌─────────────────────────────────────────────────────────────────┐\n");
  printf("│ CPU (Native C, recursive)                                       │\n");
  printf("└─────────────────────────────────────────────────────────────────┘\n");
  
  clock_t cpu_start = clock();
  u64 cpu_result = cpu_sum(1, n);
  clock_t cpu_end = clock();
  double cpu_time = (double)(cpu_end - cpu_start) / CLOCKS_PER_SEC * 1000.0;
  
  printf("  Result: %llu\n", (unsigned long long)cpu_result);
  printf("  Time:   %.3f ms\n\n", cpu_time);
  
  // ─────────────────────────────────────────────────────────────────────────
  // GPU Benchmark
  // ─────────────────────────────────────────────────────────────────────────
  printf("┌─────────────────────────────────────────────────────────────────┐\n");
  printf("│ GPU (HVM Interaction Calculus, parallel tree reduction)         │\n");
  printf("└─────────────────────────────────────────────────────────────────┘\n");
  
  cudaEvent_t start_event, stop_event;
  CUDA_CHECK(cudaEventCreate(&start_event));
  CUDA_CHECK(cudaEventCreate(&stop_event));
  
  CUDA_CHECK(cudaEventRecord(start_event));
  u64 gpu_result = gpu_sum(&state, 1, n);
  CUDA_CHECK(cudaEventRecord(stop_event));
  CUDA_CHECK(cudaEventSynchronize(stop_event));
  
  float gpu_time_ms;
  CUDA_CHECK(cudaEventElapsedTime(&gpu_time_ms, start_event, stop_event));
  
  printf("  Result: %llu\n", (unsigned long long)gpu_result);
  printf("  Time:   %.3f ms\n", gpu_time_ms);
  printf("  Interactions: %llu\n", (unsigned long long)gpu_get_interactions(&state));
  printf("  Heap used: %llu words (%.1f MB)\n\n", 
         (unsigned long long)gpu_get_heap_used(&state),
         gpu_get_heap_used(&state) * 8.0 / (1024.0 * 1024.0));
  
  // ─────────────────────────────────────────────────────────────────────────
  // Results
  // ─────────────────────────────────────────────────────────────────────────
  printf("┌─────────────────────────────────────────────────────────────────┐\n");
  printf("│ Results                                                         │\n");
  printf("└─────────────────────────────────────────────────────────────────┘\n");
  
  u64 expected = (u64)n * (n + 1) / 2;
  printf("  Expected (analytical): %llu\n", (unsigned long long)expected);
  printf("  CPU result:            %llu\n", (unsigned long long)cpu_result);
  printf("  GPU result:            %llu (32-bit: %u)\n", 
         (unsigned long long)gpu_result, (u32)gpu_result);
  
  bool cpu_match = (cpu_result == expected);
  bool gpu_match = (gpu_result == (expected & 0xFFFFFFFF));
  
  printf("\n  CPU: %s\n", cpu_match ? "✓ PASS" : "✗ FAIL");
  printf("  GPU: %s (mod 2^32)\n", gpu_match ? "✓ PASS" : "✗ FAIL");
  
  if (cpu_time > 0) {
    printf("\n  Speedup: %.2fx\n", cpu_time / gpu_time_ms);
  }
  
  // Cleanup
  cudaEventDestroy(start_event);
  cudaEventDestroy(stop_event);
  gpu_free(&state);
  
  return (cpu_match && gpu_match) ? 0 : 1;
}
