# Tree Sum Example - Demonstrates HVM's Parallel Forking
#
# This example shows dramatic GPU speedup because:
# 1. `bend` creates exponential parallelism (2^depth nodes)
# 2. `fold` reduces the tree in parallel
# 3. No data dependencies between branches

type MyTree:
  Node { val: u24, ~left: MyTree, ~right: MyTree }
  Leaf

#{
  Sum all the values in the tree.
  The fold is implicitly called for fields marked with '~'.
#}
def MyTree.sum(x: MyTree) -> u24:
  fold x:
    case MyTree/Node:
      return x.val + x.left + x.right
    case MyTree/Leaf:
      return 0

#{
  Build a complete binary tree of given depth.
  Each node's value is its depth level.
  Total nodes = 2^depth - 1
#}
def main() -> u24:
  # Build tree with bend (parallel recursive construction)
  bend depth = 0:
    when depth < 24:  # 2^24 = 16M nodes!
      x = MyTree/Node { val: 1, left: fork(depth + 1), right: fork(depth + 1) }
    else:
      x = MyTree/Leaf

  # Sum tree with fold (parallel reduction)
  return MyTree.sum(x)

# Expected result: 2^24 - 1 = 16,777,215 (each internal node contributes 1)
