# Hello World in Bend

def main():
  return "Hello, Bend!"

# Identity function
def identity(x):
  return x

# Constant function
def const(x, y):
  return x

# Function application
def apply(f, x):
  return f(x)

# Function composition
def compose(f, g, x):
  return f(g(x))

# Flip arguments
def flip(f, x, y):
  return f(y, x)

# Simple arithmetic
def add(x, y):
  return x + y

def mul(x, y):
  return x * y

def square(x):
  return x * x

# Example usage
def example():
  let double = lambda x: x * 2
  let triple = lambda x: x * 3
  let six_times = compose(double, triple)
  return six_times(7)  # Returns 42
