# Tree operations in Bend
# Demonstrates type definitions and fold

type Tree:
  case Node:
    ~left: Tree   # ~ marks recursive fields
    ~right: Tree
  case Leaf:
    value: U24

# Sum all values in a tree using fold
# Fold automatically recurses on fields marked with ~
def sum(tree: Tree) -> U24:
  fold tree:
    case Tree/Node:
      return tree.left + tree.right
    case Tree/Leaf:
      return tree.value

# Count leaves
def count(tree: Tree) -> U24:
  fold tree:
    case Tree/Node:
      return tree.left + tree.right
    case Tree/Leaf:
      return 1

# Find maximum value
def max(a: U24, b: U24) -> U24:
  if a > b:
    return a
  else:
    return b

def tree_max(tree: Tree) -> U24:
  fold tree:
    case Tree/Node:
      return max(tree.left, tree.right)
    case Tree/Leaf:
      return tree.value

# Map over leaves
def map_tree(tree: Tree, f) -> Tree:
  fold tree:
    case Tree/Node:
      return Tree/Node { left: tree.left, right: tree.right }
    case Tree/Leaf:
      return Tree/Leaf { value: f(tree.value) }

# Build a balanced tree of depth n
def build_tree(depth: U24, val: U24) -> Tree:
  switch depth:
    case 0:
      return Tree/Leaf { value: val }
    case _:
      return Tree/Node {
        left: build_tree(depth - 1, val * 2),
        right: build_tree(depth - 1, val * 2 + 1)
      }

# Test
def main():
  let tree = Tree/Node {
    left: Tree/Leaf { value: 1 },
    right: Tree/Node {
      left: Tree/Leaf { value: 2 },
      right: Tree/Leaf { value: 3 }
    }
  }
  return sum(tree)  # Returns 6
