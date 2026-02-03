// Fibonacci: compute fib(n) with a simple loop.
// Exercises: variables, for-range, arithmetic, print with format.
main :: () {
  n: int = 10;
  a: int = 0;
  b: int = 1;
  for i : 0..n - 1 {
    next: int = a + b;
    a = b;
    b = next;
  }
  print("fib(%lld) = %lld\n", n, a);
}
