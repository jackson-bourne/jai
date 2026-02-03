// Declare struct and enum (they appear in _type_table); no instance yet.
Point :: struct {
  x: float;
  y: float;
}

Color :: enum u16 {
  RED,
  GREEN,
  BLUE,
}

main :: () {
  print("Type table (struct Point, enum Color should appear):\n");
  for _type_table {
    print("  [%lld] %s (tag %d)\n", it_index, it.name, it.type);
  }
}
