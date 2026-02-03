// Multi-file: #load another file and use its constants.
#load "loaded.jai";

main :: () {
  print("SHARED_CONST = %lld\n", SHARED_CONST);
  print("EXTRA = %lld\n", EXTRA);
  print("SHARED_CONST + EXTRA = %lld\n", SHARED_CONST + EXTRA);
}
