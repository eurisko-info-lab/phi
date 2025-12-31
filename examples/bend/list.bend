# List operations in Bend
# Demonstrates pattern matching on built-in list type

# List/Nil and List/Cons are built-in

# Length of list
def length(xs):
  fold xs:
    case List/Cons:
      return 1 + xs.tail
    case List/Nil:
      return 0

# Map function over list
def map(xs, f):
  fold xs:
    case List/Cons:
      return List/Cons { head: f(xs.head), tail: xs.tail }
    case List/Nil:
      return List/Nil

# Filter list by predicate
def filter(xs, p):
  fold xs:
    case List/Cons:
      if p(xs.head):
        return List/Cons { head: xs.head, tail: xs.tail }
      else:
        return xs.tail
    case List/Nil:
      return List/Nil

# Fold right
def foldr(xs, f, z):
  fold xs:
    case List/Cons:
      return f(xs.head, xs.tail)
    case List/Nil:
      return z

# Fold left
def foldl(xs, f, acc):
  match xs:
    case List/Cons:
      return foldl(xs.tail, f, f(acc, xs.head))
    case List/Nil:
      return acc

# Sum of list
def sum(xs):
  foldr(xs, lambda a, b: a + b, 0)

# Product of list
def product(xs):
  foldr(xs, lambda a, b: a * b, 1)

# Concatenate two lists
def concat(xs, ys):
  fold xs:
    case List/Cons:
      return List/Cons { head: xs.head, tail: xs.tail }
    case List/Nil:
      return ys

# Reverse list
def reverse(xs):
  foldl(xs, lambda acc, x: List/Cons { head: x, tail: acc }, List/Nil)

# Take first n elements
def take(n, xs):
  switch n:
    case 0:
      return List/Nil
    case _:
      match xs:
        case List/Cons:
          return List/Cons { head: xs.head, tail: take(n - 1, xs.tail) }
        case List/Nil:
          return List/Nil

# Drop first n elements
def drop(n, xs):
  switch n:
    case 0:
      return xs
    case _:
      match xs:
        case List/Cons:
          return drop(n - 1, xs.tail)
        case List/Nil:
          return List/Nil

# Range [start, end)
def range(start: U24, end: U24):
  bend i = start:
    when i < end:
      return List/Cons { head: i, tail: fork(i + 1) }
    else:
      return List/Nil

# Quicksort
def quicksort(xs):
  match xs:
    case List/Nil:
      return List/Nil
    case List/Cons:
      let pivot = xs.head
      let smaller = filter(xs.tail, lambda x: x < pivot)
      let larger = filter(xs.tail, lambda x: x >= pivot)
      return concat(quicksort(smaller), List/Cons { head: pivot, tail: quicksort(larger) })

# Test
def main():
  let xs = [5, 2, 8, 1, 9, 3]
  return quicksort(xs)  # Returns [1, 2, 3, 5, 8, 9]
