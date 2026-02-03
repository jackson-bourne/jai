#include <gtest/gtest.h>
#include "lexer.hpp"
#include <string>
#include <vector>

namespace {

std::vector<jai::Token> lex_all(const std::string& source) {
  std::vector<jai::Token> tokens;
  jai::Lexer lexer(source);
  for (;;) {
    jai::Token t = lexer.next();
    tokens.push_back(t);
    if (t.kind == jai::TokenKind::EndOfFile) break;
  }
  return tokens;
}

std::vector<jai::TokenKind> lex_kinds(const std::string& source) {
  auto tokens = lex_all(source);
  std::vector<jai::TokenKind> kinds;
  for (const auto& t : tokens) kinds.push_back(t.kind);
  return kinds;
}

}  // namespace

TEST(Lexer, EmptyInput) {
  auto kinds = lex_kinds("");
  ASSERT_EQ(kinds.size(), 1u);
  EXPECT_EQ(kinds[0], jai::TokenKind::EndOfFile);
}

TEST(Lexer, WhitespaceOnly) {
  auto kinds = lex_kinds("   \n\t  ");
  ASSERT_EQ(kinds.size(), 1u);
  EXPECT_EQ(kinds[0], jai::TokenKind::EndOfFile);
}

TEST(Lexer, IntLiteral) {
  auto tokens = lex_all("42");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::IntLiteral);
  EXPECT_EQ(tokens[0].text, "42");
  EXPECT_EQ(tokens[1].kind, jai::TokenKind::EndOfFile);
}

TEST(Lexer, IntLiteralZero) {
  auto tokens = lex_all("0");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::IntLiteral);
  EXPECT_EQ(tokens[0].text, "0");
}

TEST(Lexer, FloatLiteral) {
  auto tokens = lex_all("0.5");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::FloatLiteral);
  EXPECT_EQ(tokens[0].text, "0.5");
}

TEST(Lexer, FloatLiteralScientific) {
  auto tokens = lex_all("1e-1");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::FloatLiteral);
  EXPECT_EQ(tokens[0].text, "1e-1");
}

TEST(Lexer, StringLiteralDouble) {
  auto tokens = lex_all(R"("hello")");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::StringLiteral);
  EXPECT_EQ(tokens[0].text, "\"hello\"");
}

TEST(Lexer, StringLiteralSingle) {
  auto tokens = lex_all("'a'");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::StringLiteral);
  EXPECT_EQ(tokens[0].text, "'a'");
}

TEST(Lexer, Keywords) {
  EXPECT_EQ(lex_kinds("struct")[0], jai::TokenKind::KwStruct);
  EXPECT_EQ(lex_kinds("enum")[0], jai::TokenKind::KwEnum);
  EXPECT_EQ(lex_kinds("for")[0], jai::TokenKind::KwFor);
  EXPECT_EQ(lex_kinds("loop")[0], jai::TokenKind::KwLoop);
  EXPECT_EQ(lex_kinds("if")[0], jai::TokenKind::KwIf);
  EXPECT_EQ(lex_kinds("else")[0], jai::TokenKind::KwElse);
  EXPECT_EQ(lex_kinds("while")[0], jai::TokenKind::Identifier);  // Jai has no while keyword
  EXPECT_EQ(lex_kinds("return")[0], jai::TokenKind::KwReturn);
  EXPECT_EQ(lex_kinds("inline")[0], jai::TokenKind::KwInline);
  EXPECT_EQ(lex_kinds("no_inline")[0], jai::TokenKind::KwNoInline);
  EXPECT_EQ(lex_kinds("defer")[0], jai::TokenKind::KwDefer);
  EXPECT_EQ(lex_kinds("new")[0], jai::TokenKind::KwNew);
  EXPECT_EQ(lex_kinds("delete")[0], jai::TokenKind::KwDelete);
  EXPECT_EQ(lex_kinds("cast")[0], jai::TokenKind::KwCast);
  EXPECT_EQ(lex_kinds("then")[0], jai::TokenKind::KwThen);
  EXPECT_EQ(lex_kinds("true")[0], jai::TokenKind::KwTrue);
  EXPECT_EQ(lex_kinds("false")[0], jai::TokenKind::KwFalse);
  EXPECT_EQ(lex_kinds("null")[0], jai::TokenKind::KwNull);
  EXPECT_EQ(lex_kinds("SOA")[0], jai::TokenKind::KwSOA);
  EXPECT_EQ(lex_kinds("Any")[0], jai::TokenKind::KwAny);
  EXPECT_EQ(lex_kinds("type")[0], jai::TokenKind::KwType);
  EXPECT_EQ(lex_kinds("proc")[0], jai::TokenKind::KwProc);
  EXPECT_EQ(lex_kinds("it")[0], jai::TokenKind::KwIt);
}

TEST(Lexer, Identifier) {
  auto tokens = lex_all("foo");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::Identifier);
  EXPECT_EQ(tokens[0].text, "foo");
}

TEST(Lexer, IdentifierNotKeyword) {
  auto tokens = lex_all("structs");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::Identifier);
  EXPECT_EQ(tokens[0].text, "structs");
}

TEST(Lexer, SingleCharOperators) {
  EXPECT_EQ(lex_kinds(";")[0], jai::TokenKind::Semicolon);
  EXPECT_EQ(lex_kinds(",")[0], jai::TokenKind::Comma);
  EXPECT_EQ(lex_kinds(".")[0], jai::TokenKind::Dot);
  EXPECT_EQ(lex_kinds(":")[0], jai::TokenKind::Colon);
  EXPECT_EQ(lex_kinds("?")[0], jai::TokenKind::Question);
  EXPECT_EQ(lex_kinds("=")[0], jai::TokenKind::Eq);
  EXPECT_EQ(lex_kinds("+")[0], jai::TokenKind::Plus);
  EXPECT_EQ(lex_kinds("-")[0], jai::TokenKind::Minus);
  EXPECT_EQ(lex_kinds("*")[0], jai::TokenKind::Star);
  EXPECT_EQ(lex_kinds("/")[0], jai::TokenKind::Slash);
  EXPECT_EQ(lex_kinds("%")[0], jai::TokenKind::Percent);
  EXPECT_EQ(lex_kinds("^")[0], jai::TokenKind::Caret);
  EXPECT_EQ(lex_kinds("~")[0], jai::TokenKind::Tilde);
  EXPECT_EQ(lex_kinds("@")[0], jai::TokenKind::At);
  EXPECT_EQ(lex_kinds("$")[0], jai::TokenKind::Dollar);
  EXPECT_EQ(lex_kinds("(")[0], jai::TokenKind::LParen);
  EXPECT_EQ(lex_kinds(")")[0], jai::TokenKind::RParen);
  EXPECT_EQ(lex_kinds("[")[0], jai::TokenKind::LBracket);
  EXPECT_EQ(lex_kinds("]")[0], jai::TokenKind::RBracket);
  EXPECT_EQ(lex_kinds("{")[0], jai::TokenKind::LBrace);
  EXPECT_EQ(lex_kinds("}")[0], jai::TokenKind::RBrace);
  EXPECT_EQ(lex_kinds("<")[0], jai::TokenKind::Less);
  EXPECT_EQ(lex_kinds(">")[0], jai::TokenKind::Greater);
  EXPECT_EQ(lex_kinds("&")[0], jai::TokenKind::Amp);
  EXPECT_EQ(lex_kinds("|")[0], jai::TokenKind::Pipe);
  EXPECT_EQ(lex_kinds("!")[0], jai::TokenKind::Exclaim);
}

TEST(Lexer, MultiCharOperators) {
  // No space between - and > so we get Arrow, not Minus Greater
  auto tokens = lex_all("::->..---");
  ASSERT_GE(tokens.size(), 5u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::ColonColon);
  EXPECT_EQ(tokens[0].text, "::");
  EXPECT_EQ(tokens[1].kind, jai::TokenKind::Arrow);
  EXPECT_EQ(tokens[1].text, "->");
  EXPECT_EQ(tokens[2].kind, jai::TokenKind::DotDot);
  EXPECT_EQ(tokens[2].text, "..");
  EXPECT_EQ(tokens[3].kind, jai::TokenKind::Ellipsis);
  EXPECT_EQ(tokens[3].text, "---");
}

TEST(Lexer, DynamicArray) {
  auto tokens = lex_all("[..]");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::DynamicArray);
  EXPECT_EQ(tokens[0].text, "[..]");
}

TEST(Lexer, EqEqNotEqLessEqGreaterEq) {
  auto kinds = lex_kinds("== != <= >=");
  ASSERT_GE(kinds.size(), 5u);
  EXPECT_EQ(kinds[0], jai::TokenKind::EqEq);
  EXPECT_EQ(kinds[1], jai::TokenKind::NotEq);
  EXPECT_EQ(kinds[2], jai::TokenKind::LessEq);
  EXPECT_EQ(kinds[3], jai::TokenKind::GreaterEq);
}

TEST(Lexer, AndOrShift) {
  auto kinds = lex_kinds("&& || << >>");
  ASSERT_GE(kinds.size(), 5u);
  EXPECT_EQ(kinds[0], jai::TokenKind::AndAnd);
  EXPECT_EQ(kinds[1], jai::TokenKind::OrOr);
  EXPECT_EQ(kinds[2], jai::TokenKind::LeftShift);
  EXPECT_EQ(kinds[3], jai::TokenKind::RightShift);
}

TEST(Lexer, Directives) {
  auto tokens = lex_all("#run");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::DirRun);
  EXPECT_EQ(tokens[0].text, "#run");

  tokens = lex_all("#load");
  ASSERT_GE(tokens.size(), 2u);
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::DirLoad);

  tokens = lex_all("#inline");
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::DirInline);

  tokens = lex_all("#no_inline");
  EXPECT_EQ(tokens[0].kind, jai::TokenKind::DirNoInline);
}

TEST(Lexer, LineComment) {
  auto kinds = lex_kinds("foo // bar 42\nbaz");
  ASSERT_GE(kinds.size(), 3u);
  EXPECT_EQ(kinds[0], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[1], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[2], jai::TokenKind::EndOfFile);
}

TEST(Lexer, BlockComment) {
  auto kinds = lex_kinds("foo /* bar */ baz");
  ASSERT_GE(kinds.size(), 3u);
  EXPECT_EQ(kinds[0], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[1], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[2], jai::TokenKind::EndOfFile);
}

TEST(Lexer, NestedBlockComment) {
  auto kinds = lex_kinds("a /* b /* c */ d */ e");
  ASSERT_GE(kinds.size(), 3u);
  EXPECT_EQ(kinds[0], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[1], jai::TokenKind::Identifier);
  EXPECT_EQ(kinds[2], jai::TokenKind::EndOfFile);
}

TEST(Lexer, Exclaim) {
  auto kinds = lex_kinds("!");
  ASSERT_GE(kinds.size(), 2u);
  EXPECT_EQ(kinds[0], jai::TokenKind::Exclaim);
}
