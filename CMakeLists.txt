cmake_minimum_required(VERSION 3.20)
project(jai LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)

# Allow any linker: forward standard env flags (e.g. LDFLAGS) if set
if(DEFINED ENV{LDFLAGS})
  string(APPEND CMAKE_EXE_LINKER_FLAGS " $ENV{LDFLAGS}")
  string(APPEND CMAKE_SHARED_LINKER_FLAGS " $ENV{LDFLAGS}")
endif()

find_package(LLVM REQUIRED CONFIG)

# Google Test
include(FetchContent)
FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG v1.17.0
)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)
enable_testing()

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(JAI_LIB_SOURCES
  src/lexer.cpp
  src/parser.cpp
  src/ast_dump.cpp
  src/types.cpp
  src/sema.cpp
  src/codegen.cpp
  src/interpreter.cpp
  src/build_config.cpp
)
add_library(jai_lib STATIC ${JAI_LIB_SOURCES})
target_include_directories(jai_lib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_definitions(jai_lib PUBLIC ${LLVM_DEFINITIONS_LIST})

llvm_map_components_to_libnames(jai_llvm_libs
  Support
  Core
  IRReader
  MC
  Target
  X86CodeGen
  TransformUtils
  Analysis
)
target_link_libraries(jai_lib PUBLIC ${jai_llvm_libs})

# Compiler executable
add_executable(jai src/main.cpp)
target_link_libraries(jai PRIVATE jai_lib)

# Unit test executable
set(JAI_TEST_SOURCES
  tests/test_lexer.cpp
  tests/test_parser.cpp
  tests/test_sema.cpp
  tests/test_integration.cpp
)
add_executable(jai_test ${JAI_TEST_SOURCES})
target_link_libraries(jai_test PRIVATE jai_lib GTest::gtest GTest::gtest_main)
# Run from source dir so examples/hello.jai etc. resolve; pass compiler path for integration test
add_test(NAME jai_test
  COMMAND jai_test
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)
set_tests_properties(jai_test PROPERTIES
  ENVIRONMENT "JAI_BIN=$<TARGET_FILE:jai>"
)
