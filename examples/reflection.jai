// Iterate _type_table and print type index and name (reflection/RTTI).
main :: () {
  print("Type table (%lld entries):\n", _type_table.count);
  for _type_table {
    print("  %lld: %s\n", it_index, it.name);
  }
}
