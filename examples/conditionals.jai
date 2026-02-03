// If/else branches and expression-based conditions.
main :: () {
  x: int = 10;
  y: int = 20;

  if x < y {
    print("x < y\n");
  } else {
    print("x >= y\n");
  }

  if x + y == 30 {
    print("x + y == 30\n");
  }

  z: int = 0;
  if x > 5 {
    z = 1;
  } else if (x == 10) {
    z = 2;
  } else {
    z = 3;
  }
  print("z = %lld\n", z);
}
