# Jai compiler

A compiler for a subset of the [Jai](https://jai.community/) language. It compiles `.jai` source to LLVM IR and links with Clang to produce native executables.

## Prerequisites

- **CMake** 3.20+
- **LLVM** (with development headers; found via `find_package(LLVM)`)
- **C++20** compiler (for building the compiler)
- **Clang** on `PATH` (used to link the generated IR into an executable)

## Building

From the project root:

```bash
mkdir -p build && cd build
cmake ..
make -j
```

This produces:

- **`build/jai`** – compiler executable  
- **`build/jai_test`** – unit/integration test binary  

## Using the compiler

```bash
# Compile and produce an executable
./build/jai examples/hello.jai -o my_program
./my_program

# Emit LLVM IR only (no -o): writes <name>.ll next to the binary or output.ll
./build/jai examples/hello.jai

# Dump AST and exit
./build/jai --dump-ast examples/hello.jai

# Help
./build/jai --help
```

### Quick run script

From the repo root you can compile and run an example in one step:

```bash
./run_example hello
# or
./run_example examples/conditionals.jai
```

The script compiles to a temp binary and runs it; pass extra args after the example name if needed.

## Language overview

Programs consist of declarations at file scope and a `main` procedure that takes no arguments:

```jai
main :: () {
  x: int = 10;
  print("x = %lld\n", x);
}
```

- **Variables:** `name: type = value;` or `name := value;` (type inferred).  
- **Procedures:** `name :: (params) -> return_type { ... }` or `name :: () { ... }` for no return.  
- **Control flow:** `if cond { ... } else { ... }`, `for i : start..end { ... }`, `for container { ... }` (with `it` and `it_index` inside the loop).  
- **Built-ins:** `print("format", ...)` (printf-style); `_type_table` and `for _type_table` for reflection.  
- **Compile-time:** `#run expr()` to run code at compile time; `#load "file.jai"` to include another file.

## Examples

Example programs are in **`examples/`**. A few entry points:

| Example        | Description                          |
|----------------|--------------------------------------|
| `hello.jai`    | Minimal program, prints "Hello"     |
| `conditionals.jai` | If/else and variable assignment |
| `reflection.jai`   | Iterate type table with `it`, `it_index` |
| `compile_time_compute.jai` | `#run` and compile-time arrays |
| `load_test.jai`    | `#load` and multi-file constants   |

See [examples/README.md](examples/README.md) for a full list and notes.

## Testing

Run the test suite (from repo root, so `examples/` and paths resolve):

```bash
cd build
ctest
# or run the test binary directly (set JAI_BIN so integration tests can find the compiler):
JAI_BIN=$PWD/jai ./jai_test
```

Tests cover lexer, parser, sema, and integration (compile + run example programs).

## Project layout

```
jai/
├── CMakeLists.txt    # Build: jai compiler, jai_lib, jai_test
├── run_example       # Script: compile + run an example
├── src/              # Compiler source (lexer, parser, sema, codegen, …)
├── examples/         # Sample .jai programs
└── tests/            # C++ unit/integration tests (Google Test)
```

Generated build artifacts (e.g. `build/`, `output.ll`) are listed in [.gitignore](.gitignore).
