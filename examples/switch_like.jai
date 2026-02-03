// Many if/else branches to stress conditionals and codegen.
main :: () {
  x: int = 2;
  result: int = 0;
  if x == 0 { result = 10; }
  else if x == 1 { result = 20; }
  else if x == 2 { result = 30; }
  else if x == 3 { result = 40; }
  else { result = 99; }
  print("result = %lld\n", result);
}
