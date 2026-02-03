# Jai compiler tests

C++ unit and integration tests using Google Test.

## Run tests

From the project root after building:

```bash
./build/jai_test
```

Or with ctest (set `JAI_BIN` so the integration test can compile an example):

```bash
cd build && JAI_BIN=$PWD/jai ctest --output-on-failure
```

## Layout

- **tests/** – test sources
  - `test_lexer.cpp` – lexer (tokens, keywords, directives, comments)
  - `test_parser.cpp` – parser (declarations, statements, expressions, types)
  - `test_sema.cpp` – semantic analysis (scopes, `it` in for-iter, types)
  - `test_integration.cpp` – compiles and runs `examples/hello.jai`

Example Jai programs live in **examples/** (see `examples/README.md`).
