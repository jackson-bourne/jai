// Example: defining and calling procedures.
// Functions can be referenced out of order (no forward declarations needed).

main :: () {
  greet();
  a: int = double(7);
  print("double(7) = %lld\n", a);
  print("double(21) = %lld\n", double(21));
}

greet :: () {
  print("Hello from greet()\n");
}

double :: (x: int) -> int {
  return x * 2;
}
