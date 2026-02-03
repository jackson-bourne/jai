// Same as reflection: for with print(it_index, it.name) inside
main :: () {
  for _type_table {
    print("  %lld: %s\n", it_index, it.name);
  }
}
