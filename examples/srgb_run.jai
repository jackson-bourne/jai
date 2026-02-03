SRGB_TABLE_SIZE :: 8;

generate_table :: () -> [] float {
  srgb_table: [SRGB_TABLE_SIZE] float;
  for i : 0..SRGB_TABLE_SIZE - 1 {
    srgb_table[i] = cast(float) i * 0.5;
  }
  return srgb_table;
}

table: [] float = #run generate_table();

main :: () {
  // printf format: %f = float, %lld = int64 (C format specifiers)
  print("first: %f\n", table[0]);
  print("count: %lld\n", table.count);
  print("table: ");
  for i : 0..SRGB_TABLE_SIZE - 1 {
    print("%f ", table[i]);
  }
  print("\n");
}
