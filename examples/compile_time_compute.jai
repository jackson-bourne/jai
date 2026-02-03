// More involved #run: build a small lookup table at compile time, use at runtime.
SIZE :: 5;

make_squares :: () -> [] float {
  arr: [SIZE] float;
  for i : 0..SIZE - 1 {
    arr[i] = cast(float) (i * i);
  }
  return arr;
}

squares: [] float = #run make_squares();

main :: () {
  print("Squares 0..4: ");
  for i : 0..SIZE - 1 {
    print("%f ", squares[i]);
  }
  print("\n");
  print("squares.count = %lld\n", squares.count);
}
