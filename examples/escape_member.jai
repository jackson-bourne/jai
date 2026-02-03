// it and it_index are only defined inside a for loop (e.g. for _type_table).
// Using them outside the loop is invalid; the compiler reports an error.
// Correct usage (same print as in reflection.jai, inside the loop):
main :: () {
  for _type_table {
    print("  %lld: %s\n", it_index, it.name);
  }
}
