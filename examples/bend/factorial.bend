# Factorial in Bend
# Demonstrates switch for numeric pattern matching

def factorial(n: U24) -> U24:
  switch n:
    case 0:
      return 1
    case _:
      return n * factorial(n - 1)

# Tail-recursive factorial
def factorial_tail(n: U24, acc: U24) -> U24:
  switch n:
    case 0:
      return acc
    case _:
      return factorial_tail(n - 1, n * acc)

def factorial2(n: U24) -> U24:
  return factorial_tail(n, 1)

# Test
def main():
  return factorial(5)  # Returns 120
