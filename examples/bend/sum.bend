# Parallel Sum Example
# Demonstrates how Bend's parallel recursion maps to HVM's superpositions

# Defines the function Sum with two parameters: start and target
def Sum(start, target):
  if start == target:
    # Base case: single element
    return start
  else:
    # Recursive case: divide and conquer
    half = (start + target) / 2
    left = Sum(start, half)        # First half
    right = Sum(half + 1, target)  # Second half
    return left + right

# A parallelizable sum of numbers from 1 to 1_000_000
# Result: 500000500000 (but we'll use smaller numbers for testing)
def main():
  return Sum(1, 1000000)
