// Nested loops and arithmetic: sum of products, simple 2D iteration.
main :: () {
  sum: int = 0;
  for i : 0..3 {
    for j : 0..3 {
      sum = sum + i * j;
    }
  }
  // 0*0+0*1+...+3*3 = 36
  print("sum of i*j for i,j in 0..3: %lld\n", sum);

  prod: int = 1;
  for k : 1..5 {
    prod = prod * k;
  }
  print("5! = %lld\n", prod);
}
