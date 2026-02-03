# Jai example programs

Build the compiler from the project root, then run e.g.:
`./build/jai examples/<name>.jai -o /tmp/out && /tmp/out`

## Programs that work end-to-end

- **hello.jai** – minimal print
- **add.jai** – empty main (compile + run)
- **srgb_run.jai** – `#run` build(), compile-time array, for-range, print with format
- **load_test.jai** / **multi_file.jai** – `#load` and shared constants (SHARED_CONST, EXTRA)
- **loaded.jai** – used by load_test / multi_file (constants only; no main, link only when used via #load)
- **compile_time_compute.jai** – `#run` returning float array, .count
- **simple_var.jai** – file-scope `x : int = 0`, main prints it
- **reflection.jai** – `for _type_table { print(..., it.name); }` (parses and compiles)
- **for_member.jai** – same as reflection; `it.name` in for-iter body
- **for_member_nospace.jai**, **for_member_foo.jai** – for-iter with it.name variants

## Programs that compile but have known wrong output

- **nested.jai** – nested for-range, sum of products; currently wrong sum (e.g. 9 instead of 36) and wrong factorial.
- **conditionals.jai** – if/else; variable `z` printed after assignment in branches shows garbage.
- **switch_like.jai** – chained if/else; `result` shows garbage.
- **fibonacci.jai** – loop-based fib; output is wrong (e.g. fib(10)=1 instead of 55).
- **sum_simple.jai** – sum 0..n; sum is wrong (e.g. 5 instead of 15).
- **srgb_minimal.jai** – main calls generate_table(); only main is emitted so call is undefined/wrong output.
- **for_member.jai** / **reflection.jai** – run but it.name can print garbage (type-info name pointer).

## Programs that fail to compile

- **for_it_only.jai** – single `it;` in for _type_table body; sema reports undefined identifier `it`.

## Minimal / debugging

- **mini.jai**, **two_arg_member.jai**, **three_arg_member.jai**, **escape_member.jai** – minimal prints / it.name outside for-iter.
- **struct_and_enum.jai** – struct/enum declarations, for _type_table with it.name, it.type.
