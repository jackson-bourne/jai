#include <gtest/gtest.h>
#include "parser.hpp"
#include "sema.hpp"
#include <string>
#include <set>

namespace {

bool parse_and_sema_ok(const std::string& source) {
  jai::Parser p(source, "<test>");
  std::unique_ptr<jai::File> file = p.parse_file();
  if (p.has_errors()) return false;
  if (!file) return false;
  jai::SemaContext sema;
  return sema.check_file(*file);
}

bool parse_and_sema_fails(const std::string& source) {
  jai::Parser p(source, "<test>");
  std::unique_ptr<jai::File> file = p.parse_file();
  if (p.has_errors()) return true;
  if (!file) return true;
  jai::SemaContext sema;
  return !sema.check_file(*file);
}

void expect_sema_error_containing(const std::string& source, const std::string& substring) {
  jai::Parser p(source, "<test>");
  std::unique_ptr<jai::File> file = p.parse_file();
  ASSERT_FALSE(p.has_errors());
  ASSERT_TRUE(file);
  jai::SemaContext sema;
  sema.check_file(*file);
  ASSERT_FALSE(sema.errors.empty()) << "Expected sema error containing \"" << substring << "\"";
  bool found = false;
  for (const auto& err : sema.errors) {
    if (err.find(substring) != std::string::npos) { found = true; break; }
  }
  EXPECT_TRUE(found) << "No error contained \"" << substring << "\". Errors: " << sema.errors[0];
}

}  // namespace

TEST(Sema, ValidVarDecl) {
  EXPECT_TRUE(parse_and_sema_ok("x: int = 0;"));
}

TEST(Sema, ValidProc) {
  EXPECT_TRUE(parse_and_sema_ok(
      "main :: () { return; };"));
}

TEST(Sema, ValidProcWithParam) {
  EXPECT_TRUE(parse_and_sema_ok(
      "f :: (x: int) -> int { return x; };"));
}

TEST(Sema, UndefinedIdentifier) {
  expect_sema_error_containing(
      "main :: () { y; };",
      "undefined");
}

TEST(Sema, ForIterIt) {
  EXPECT_TRUE(parse_and_sema_ok(
      "main :: () { for _type_table { it; } };"));
}

TEST(Sema, ForIterItIndex) {
  EXPECT_TRUE(parse_and_sema_ok(
      "main :: () { for _type_table { it_index; } };"));
}

TEST(Sema, TypeResolutionBuiltin) {
  EXPECT_TRUE(parse_and_sema_ok(
      "a: int = 0; b: float = 0.0; c: bool = true;"));
}

TEST(Sema, StructType) {
  EXPECT_TRUE(parse_and_sema_ok(
      "S :: struct { x: int; };\n"
      "v: S;\n"));
}

TEST(Sema, EnumType) {
  EXPECT_TRUE(parse_and_sema_ok(
      "E :: enum { A, B };\n"
      "e: E;\n"));
}
