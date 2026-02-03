// Simple sum 0+1+2+...+n with one loop. Easy to verify.
main :: () {
  n: int = 5;
  sum: int = 0;
  for i : 0..n {
    sum = sum + i;
  }
  // 0+1+2+3+4+5 = 15
  print("sum 0..%lld = %lld\n", n, sum);
}
