// #run with print: output should appear at compile time (when building).
foo :: () {
  print("compile-time: hello from #run\n");
  print("number: %lld\n", 42);
}

#run foo();

main :: () {
  print("runtime: hello from main\n");
}
