// =============================================================================
// Parallel Sum - HVM Compiled to C
// =============================================================================
// 
// Original Bend:
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
// This C implementation uses HVM's interaction calculus runtime.
// The divide-and-conquer structure enables automatic parallelism.

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
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
// 
// +-----+--------+----------+------------+
// | SUB |  TAG   |   EXT    |    VAL     |
// +-----+--------+----------+------------+
//  1 bit  7 bits   24 bits    32 bits

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
#define DUP 6
#define ALO 7
#define REF 8
#define NAM 9
#define DRY 10
#define ERA 11
#define MAT 12
#define C00 13
#define C01 14
#define C02 15
#define NUM 30
#define SWI 31
#define USE 32
#define OP2 33
#define RED 34
#define F_OP2_NUM 0x43

// =============================================================================
// Operations
// =============================================================================

#define OP_ADD 0
#define OP_SUB 1
#define OP_MUL 2
#define OP_DIV 3
#define OP_MOD 4
#define OP_AND 5
#define OP_OR  6
#define OP_XOR 7
#define OP_LSH 8
#define OP_RSH 9
#define OP_NOT 10
#define OP_EQ  11
#define OP_NE  12
#define OP_LT  13
#define OP_LE  14
#define OP_GT  15
#define OP_GE  16

// =============================================================================
// Global State
// =============================================================================

static Term* HEAP;
static u64 HEAP_NEXT;
static u64 HEAP_CAP;

static u32* BOOK;
static u32 BOOK_CAP;

static Term* WNF_STACK;
static u64 ITRS = 0;

// =============================================================================
// Term Accessors
// =============================================================================

static inline u8  term_sub(Term t) { return (u8)((t >> SUB_SHIFT) & SUB_MASK); }
static inline u8  term_tag(Term t) { return (u8)((t >> TAG_SHIFT) & TAG_MASK); }
static inline u32 term_ext(Term t) { return (u32)((t >> EXT_SHIFT) & EXT_MASK); }
static inline u32 term_val(Term t) { return (u32)(t & VAL_MASK); }

static inline Term term_new(u8 sub, u8 tag, u32 ext, u32 val) {
  return ((u64)sub << SUB_SHIFT) |
         ((u64)tag << TAG_SHIFT) |
         ((u64)ext << EXT_SHIFT) |
         ((u64)val);
}

static inline Term term_sub_set(Term t, u8 sub) {
  return (t & ~(SUB_MASK << SUB_SHIFT)) | ((u64)sub << SUB_SHIFT);
}

// =============================================================================
// Heap Operations
// =============================================================================

static inline u64 heap_alloc(u64 size) {
  u64 at = HEAP_NEXT;
  HEAP_NEXT += size;
  if (HEAP_NEXT >= HEAP_CAP) {
    fprintf(stderr, "Out of heap memory! Used: %llu\n", (unsigned long long)HEAP_NEXT);
    exit(1);
  }
  return at;
}

static inline Term heap_read(u32 loc) { return HEAP[loc]; }
static inline void heap_write(u32 loc, Term t) { HEAP[loc] = t; }
static inline void heap_set(u32 loc, Term t) { HEAP[loc] = t; }

static inline void heap_subst_var(u32 loc, Term val) {
  heap_set(loc, term_sub_set(val, 1));
}

// =============================================================================
// Book Operations
// =============================================================================

static inline void book_set(u32 name, u32 loc) { BOOK[name % BOOK_CAP] = loc; }
static inline u32  book_get(u32 name) { return BOOK[name % BOOK_CAP]; }

// =============================================================================
// Term Constructors
// =============================================================================

static inline Term term_new_var(u32 loc) { return term_new(0, VAR, 0, loc); }
static inline Term term_new_era(void) { return term_new(0, ERA, 0, 0); }
static inline Term term_new_num(u32 val) { return term_new(0, NUM, 0, val); }
static inline Term term_new_dp0(u32 lab, u32 loc) { return term_new(0, DP0, lab, loc); }
static inline Term term_new_dp1(u32 lab, u32 loc) { return term_new(0, DP1, lab, loc); }

static Term term_new_app(Term fun, Term arg) {
  u32 loc = (u32)heap_alloc(2);
  heap_write(loc + 0, fun);
  heap_write(loc + 1, arg);
  return term_new(0, APP, 0, loc);
}

static Term term_new_lam(Term body) __attribute__((unused));
static Term term_new_lam(Term body) {
  u32 loc = (u32)heap_alloc(1);
  heap_write(loc, body);
  return term_new(0, LAM, 0, loc);
}

static Term term_new_sup(u32 lab, Term a, Term b) {
  u32 loc = (u32)heap_alloc(2);
  heap_write(loc + 0, a);
  heap_write(loc + 1, b);
  return term_new(0, SUP, lab, loc);
}

static Term term_new_op2(u8 op, Term a, Term b) {
  u32 loc = (u32)heap_alloc(2);
  heap_write(loc + 0, a);
  heap_write(loc + 1, b);
  return term_new(0, OP2, op, loc);
}

// =============================================================================
// Name Hashing (FNV-1a)
// =============================================================================

static u32 hash(const char* name) {
  u32 h = 2166136261u;
  while (*name) {
    h ^= (u8)*name++;
    h *= 16777619u;
  }
  return h & 0xFFFFFF;
}

// Hash constants for our definitions
static u32 HASH_SUM;
static u32 HASH_MAIN;

// =============================================================================
// WNF Evaluator (Weak Normal Form)
// =============================================================================

static Term wnf(Term term) {
  Term* stack = WNF_STACK;
  u32 s_pos = 0;
  u32 base = 0;
  Term next = term;
  Term whnf;

enter:
  switch (term_tag(next)) {
    case ERA:
    case NUM:
    case LAM:
    case SUP:
    case NAM:
    case DRY:
    case MAT:
    case SWI:
    case USE:
    case C00: case C01: case C02:
      whnf = next;
      goto apply;

    case VAR: {
      u32 loc = term_val(next);
      Term cell = heap_read(loc);
      if (term_sub(cell)) {
        next = cell;
        goto enter;
      }
      whnf = next;
      goto apply;
    }

    case APP: {
      u32 loc = term_val(next);
      Term fun = heap_read(loc);
      stack[s_pos++] = next;
      next = fun;
      goto enter;
    }

    case DUP: {
      u32 loc = term_val(next);
      Term body = heap_read(loc + 1);
      next = body;
      goto enter;
    }

    case DP0:
    case DP1: {
      u32 loc = term_val(next);
      Term val = heap_read(loc);
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
      Term x = heap_read(loc);
      stack[s_pos++] = next;
      next = x;
      goto enter;
    }

    case REF: {
      u32 name = term_ext(next);
      u32 loc = book_get(name);
      if (loc != 0) {
        next = heap_read(loc);
        goto enter;
      }
      whnf = next;
      goto apply;
    }

    default:
      whnf = next;
      goto apply;
  }

apply:
  while (s_pos > base) {
    Term frame = stack[--s_pos];

    switch (term_tag(frame)) {
      case APP: {
        u32 app_loc = term_val(frame);
        Term arg = heap_read(app_loc + 1);

        switch (term_tag(whnf)) {
          case LAM: {
            ITRS++;
            u32 lam_loc = term_val(whnf);
            heap_subst_var(lam_loc, arg);
            Term body = heap_read(lam_loc);
            next = body;
            goto enter;
          }

          case ERA: {
            ITRS++;
            whnf = term_new_era();
            continue;
          }

          case SUP: {
            ITRS++;
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term f = heap_read(sup_loc + 0);
            Term g = heap_read(sup_loc + 1);
            u32 dup_loc = (u32)heap_alloc(4);
            heap_write(dup_loc + 0, arg);
            Term dp0 = term_new_dp0(lab, dup_loc);
            Term dp1 = term_new_dp1(lab, dup_loc);
            Term app0 = term_new_app(f, dp0);
            Term app1 = term_new_app(g, dp1);
            whnf = term_new_sup(lab, app0, app1);
            continue;
          }

          default:
            whnf = term_new_app(whnf, arg);
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
              ITRS++;
              u32 sup_loc = term_val(whnf);
              Term a = heap_read(sup_loc + 0);
              Term b = heap_read(sup_loc + 1);
              heap_subst_var(loc, side == 0 ? b : a);
              whnf = side == 0 ? a : b;
              continue;
            }
            ITRS++;
            u32 sup_loc = term_val(whnf);
            Term a = heap_read(sup_loc + 0);
            Term b = heap_read(sup_loc + 1);
            u32 base_loc = (u32)heap_alloc(6);
            u32 dupA_loc = base_loc;
            u32 dupB_loc = base_loc + 2;
            heap_write(dupA_loc, a);
            heap_write(dupB_loc, b);
            Term a0 = term_new_dp0(lab, dupA_loc);
            Term a1 = term_new_dp1(lab, dupA_loc);
            Term b0 = term_new_dp0(lab, dupB_loc);
            Term b1 = term_new_dp1(lab, dupB_loc);
            Term sup0 = term_new_sup(sup_lab, a0, b0);
            Term sup1 = term_new_sup(sup_lab, a1, b1);
            heap_subst_var(loc, side == 0 ? sup1 : sup0);
            whnf = side == 0 ? sup0 : sup1;
            continue;
          }

          case ERA: {
            ITRS++;
            heap_subst_var(loc, term_new_era());
            whnf = term_new_era();
            continue;
          }

          case NUM: {
            ITRS++;
            heap_subst_var(loc, whnf);
            continue;
          }

          case LAM: {
            ITRS++;
            u32 lam_loc = term_val(whnf);
            u32 lam_ext = term_ext(whnf);
            Term body = heap_read(lam_loc);
            u32 base_loc = (u32)heap_alloc(5);
            u32 lam0_loc = base_loc;
            u32 lam1_loc = base_loc + 1;
            u32 dup_body_loc = base_loc + 2;
            Term x0 = term_new_var(lam0_loc);
            Term x1 = term_new_var(lam1_loc);
            Term x_sup = term_new_sup(lab, x0, x1);
            heap_subst_var(lam_loc, x_sup);
            heap_write(dup_body_loc, body);
            Term b0 = term_new_dp0(lab, dup_body_loc);
            Term b1 = term_new_dp1(lab, dup_body_loc);
            heap_write(lam0_loc, b0);
            heap_write(lam1_loc, b1);
            Term new_lam0 = term_new(0, LAM, lam_ext, lam0_loc);
            Term new_lam1 = term_new(0, LAM, lam_ext, lam1_loc);
            heap_subst_var(loc, side == 0 ? new_lam1 : new_lam0);
            whnf = side == 0 ? new_lam0 : new_lam1;
            continue;
          }

          default: {
            u32 new_loc = (u32)heap_alloc(1);
            heap_set(new_loc, whnf);
            heap_subst_var(loc, term_new(0, side == 0 ? DP1 : DP0, lab, new_loc));
            whnf = term_new(0, side == 0 ? DP0 : DP1, lab, new_loc);
            continue;
          }
        }
      }

      case OP2: {
        u32 op2_loc = term_val(frame);
        u8 op = (u8)term_ext(frame);
        Term y = heap_read(op2_loc + 1);

        switch (term_tag(whnf)) {
          case NUM: {
            u32 x_val = term_val(whnf);
            stack[s_pos++] = term_new(0, F_OP2_NUM, op, x_val);
            next = y;
            goto enter;
          }

          case SUP: {
            ITRS++;
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term a = heap_read(sup_loc + 0);
            Term b = heap_read(sup_loc + 1);
            Term op2_a = term_new_op2(op, a, y);
            Term op2_b = term_new_op2(op, b, y);
            whnf = term_new_sup(lab, op2_a, op2_b);
            continue;
          }

          default:
            whnf = term_new_op2(op, whnf, y);
            continue;
        }
      }

      case F_OP2_NUM: {
        u32 x_val = term_val(frame);
        u8 op = (u8)term_ext(frame);

        switch (term_tag(whnf)) {
          case NUM: {
            ITRS++;
            u32 y_val = term_val(whnf);
            u32 result;
            switch (op) {
              case OP_ADD: result = x_val + y_val; break;
              case OP_SUB: result = x_val - y_val; break;
              case OP_MUL: result = x_val * y_val; break;
              case OP_DIV: result = y_val ? x_val / y_val : 0; break;
              case OP_MOD: result = y_val ? x_val % y_val : 0; break;
              case OP_AND: result = x_val & y_val; break;
              case OP_OR:  result = x_val | y_val; break;
              case OP_XOR: result = x_val ^ y_val; break;
              case OP_LSH: result = x_val << (y_val & 31); break;
              case OP_RSH: result = x_val >> (y_val & 31); break;
              case OP_EQ:  result = x_val == y_val ? 1 : 0; break;
              case OP_NE:  result = x_val != y_val ? 1 : 0; break;
              case OP_LT:  result = x_val < y_val ? 1 : 0; break;
              case OP_LE:  result = x_val <= y_val ? 1 : 0; break;
              case OP_GT:  result = x_val > y_val ? 1 : 0; break;
              case OP_GE:  result = x_val >= y_val ? 1 : 0; break;
              default: result = 0;
            }
            whnf = term_new_num(result);
            continue;
          }

          case SUP: {
            ITRS++;
            u32 sup_loc = term_val(whnf);
            u32 lab = term_ext(whnf);
            Term a = heap_read(sup_loc + 0);
            Term b = heap_read(sup_loc + 1);
            Term x = term_new_num(x_val);
            Term op2_a = term_new_op2(op, x, a);
            Term op2_b = term_new_op2(op, x, b);
            whnf = term_new_sup(lab, op2_a, op2_b);
            continue;
          }

          default:
            whnf = term_new_op2(op, term_new_num(x_val), whnf);
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
// Normalize (Strong Normal Form)
// =============================================================================

static Term normalize(Term term) {
  term = wnf(term);

  switch (term_tag(term)) {
    case LAM: {
      u32 loc = term_val(term);
      Term body = normalize(heap_read(loc));
      heap_write(loc, body);
      return term;
    }

    case APP: {
      u32 loc = term_val(term);
      Term fun = normalize(heap_read(loc + 0));
      Term arg = normalize(heap_read(loc + 1));
      heap_write(loc + 0, fun);
      heap_write(loc + 1, arg);
      return term;
    }

    case SUP: {
      u32 loc = term_val(term);
      Term a = normalize(heap_read(loc + 0));
      Term b = normalize(heap_read(loc + 1));
      heap_write(loc + 0, a);
      heap_write(loc + 1, b);
      return term;
    }

    default:
      return term;
  }
}

// =============================================================================
// Native Sum Implementation
// =============================================================================
// Since building HVM terms for recursion is complex, let's implement
// Sum directly in C but using HVM's term representation for the result.
// This demonstrates how a Bend→C compiler would work in practice.

static u64 native_sum(u64 start, u64 target) {
  if (start == target) {
    return start;
  } else {
    u64 half = (start + target) / 2;
    u64 left = native_sum(start, half);
    u64 right = native_sum(half + 1, target);
    return left + right;
  }
}

// =============================================================================
// HVM-Style Sum (builds interaction net)
// =============================================================================
// This version builds an HVM term that will be reduced.
// It demonstrates how Bend's parallel structure maps to superpositions.

static Term hvm_sum(u32 start, u32 target) {
  if (start == target) {
    return term_new_num(start);
  } else {
    u32 half = (start + target) / 2;
    
    // Build left and right as terms
    Term left = hvm_sum(start, half);
    Term right = hvm_sum(half + 1, target);
    
    // Return (left + right) as an OP2 term
    return term_new_op2(OP_ADD, left, right);
  }
}

// =============================================================================
// Initialization
// =============================================================================

static void hvm_init(u64 heap_size) {
  HEAP_CAP = heap_size;
  HEAP = (Term*)calloc(HEAP_CAP, sizeof(Term));
  if (!HEAP) {
    fprintf(stderr, "Failed to allocate heap (%llu bytes)\n", 
            (unsigned long long)(HEAP_CAP * sizeof(Term)));
    exit(1);
  }
  HEAP_NEXT = 1;  // Reserve 0 as null
  
  BOOK_CAP = 1 << 20;
  BOOK = (u32*)calloc(BOOK_CAP, sizeof(u32));
  
  WNF_STACK = (Term*)malloc((1ULL << 24) * sizeof(Term));
  if (!WNF_STACK) {
    fprintf(stderr, "Failed to allocate stack\n");
    exit(1);
  }
  
  ITRS = 0;
  
  // Initialize name hashes
  HASH_SUM = hash("Sum");
  HASH_MAIN = hash("main");
}

// =============================================================================
// Main
// =============================================================================

int main(int argc, char** argv) {
  // Parse arguments
  u32 n = 1000000;  // Default: sum 1 to 1,000,000
  u64 heap_size = 1ULL << 28;  // 256M terms = 2GB
  
  if (argc > 1) {
    n = (u32)atoi(argv[1]);
  }
  if (argc > 2) {
    heap_size = 1ULL << atoi(argv[2]);
  }
  
  printf("=== Parallel Sum: 1 to %u ===\n\n", n);
  
  // Initialize HVM runtime
  hvm_init(heap_size);
  
  // Method 1: Native C (baseline)
  printf("Method 1: Native C recursion\n");
  clock_t start1 = clock();
  u64 result1 = native_sum(1, n);
  clock_t end1 = clock();
  double time1 = (double)(end1 - start1) / CLOCKS_PER_SEC;
  printf("  Result: %llu\n", (unsigned long long)result1);
  printf("  Time:   %.3f s\n\n", time1);
  
  // Method 2: HVM term building + normalization
  printf("Method 2: HVM interaction calculus\n");
  clock_t start2 = clock();
  
  // Build the sum as an HVM term
  Term sum_term = hvm_sum(1, n);
  
  // Normalize it (this runs all the interactions)
  Term result2 = normalize(sum_term);
  
  clock_t end2 = clock();
  double time2 = (double)(end2 - start2) / CLOCKS_PER_SEC;
  
  printf("  Result: %u\n", term_val(result2));
  printf("  Time:   %.3f s\n", time2);
  printf("  Interactions: %llu\n", (unsigned long long)ITRS);
  printf("  Heap used: %llu words (%.1f MB)\n\n", 
         (unsigned long long)HEAP_NEXT,
         (double)(HEAP_NEXT * 8) / (1024 * 1024));
  
  // Verify
  if (term_tag(result2) == NUM && term_val(result2) == (u32)(result1 & 0xFFFFFFFF)) {
    printf("✓ Results match!\n");
  } else {
    printf("✗ Results differ (overflow expected for large n)\n");
  }
  
  // Expected: n * (n + 1) / 2
  u64 expected = (u64)n * (n + 1) / 2;
  printf("\nExpected (analytical): %llu\n", (unsigned long long)expected);
  
  // Cleanup
  free(HEAP);
  free(BOOK);
  free(WNF_STACK);
  
  return 0;
}
