// Example: declare external C symbols and link with raylib (or a stub).
// Build with: jai extern_raylib.jai -o extern_raylib -lraylib

@extern Raylib_add(a: int, b: int) -> int;

main :: () -> int {
  x := Raylib_add(2, 3);
  return x;
}
