# Parallel computation in Bend
# Demonstrates superpositions for automatic parallelism

# A superposition represents multiple values computed in parallel
# When both branches of a superposition are needed, they execute concurrently

# Simple superposition - returns {1 2}
def choose():
  return {1 2}

# Duplicating a superposition explores both branches
def double_choose():
  let x = choose()
  return x + x  # Both branches computed: {2 4}

# Independent operations parallelize
def parallel_add(a, b):
  return a + b

# Compute both branches of superposition in parallel
def parallel_example():
  let x = {10 20}
  let y = {1 2}
  return x + y  # Returns {11 21 12 22}

# Tree with superposition at leaves enables parallel computation
type PTree:
  case PNode:
    ~left: PTree
    ~right: PTree  
  case PLeaf:
    value: U24

# Sum with automatic parallelism
def parallel_sum(tree: PTree) -> U24:
  fold tree:
    case PTree/PNode:
      return tree.left + tree.right  # Left and right computed in parallel
    case PTree/PLeaf:
      return tree.value

# Generate parallel computation tree
def gen_parallel(depth: U24) -> PTree:
  switch depth:
    case 0:
      return PTree/PLeaf { value: 1 }
    case _:
      let sub = gen_parallel(depth - 1)
      return PTree/PNode { left: sub, right: sub }

# Parallel map over tree
def parallel_map(tree: PTree, f) -> PTree:
  fold tree:
    case PTree/PNode:
      # Both branches transformed in parallel
      return PTree/PNode { left: tree.left, right: tree.right }
    case PTree/PLeaf:
      return PTree/PLeaf { value: f(tree.value) }

# Test: builds tree of depth 4 and sums in parallel
def main():
  let tree = gen_parallel(4)
  return parallel_sum(tree)  # Returns 16
