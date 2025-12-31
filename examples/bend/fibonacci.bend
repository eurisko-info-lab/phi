# Fibonacci in Bend
# Demonstrates bend for manual recursion

# Simple recursive Fibonacci (exponential)
def fib_simple(n: U24) -> U24:
  switch n:
    case 0:
      return 0
    case _:
      switch n - 1:
        case 0:
          return 1  
        case _:
          return fib_simple(n - 1) + fib_simple(n - 2)

# Fibonacci using bend for explicit recursion control
def fib_bend(n: U24) -> U24:
  bend x = n, a = 0, b = 1:
    when x > 0:
      return fork(x - 1, b, a + b)
    else:
      return a

# Parallel Fibonacci using tree structure
type FibTree:
  case Branch:
    ~left: FibTree
    ~right: FibTree
  case Leaf:
    value: U24

def fib_tree(n: U24) -> FibTree:
  switch n:
    case 0:
      return FibTree/Leaf { value: 0 }
    case _:
      switch n - 1:
        case 0:
          return FibTree/Leaf { value: 1 }
        case _:
          return FibTree/Branch {
            left: fib_tree(n - 1),
            right: fib_tree(n - 2)
          }

def sum_fib_tree(tree: FibTree) -> U24:
  fold tree:
    case FibTree/Branch:
      return tree.left + tree.right
    case FibTree/Leaf:
      return tree.value

def fib_parallel(n: U24) -> U24:
  return sum_fib_tree(fib_tree(n))

# Test
def main():
  return fib_bend(10)  # Returns 55
