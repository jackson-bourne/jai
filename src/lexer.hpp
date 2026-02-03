#pragma once

#include <cstdint>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace jai {

struct SourceLoc {
  uint32_t line = 1;
  uint32_t column = 1;
  uint32_t offset = 0;
};

enum class TokenKind {
  EndOfFile,
  Invalid,

  // Literals
  IntLiteral,
  FloatLiteral,
  StringLiteral,

  // Identifiers and keywords
  Identifier,
  KwStruct,
  KwEnum,
  KwFor,
  KwIf,
  KwElse,
  KwWhile,
  KwReturn,
  KwInline,
  KwNoInline,
  KwDefer,
  KwNew,
  KwDelete,
  KwCast,
  KwThen,
  KwTrue,
  KwFalse,
  KwNull,
  KwSOA,
  KwAny,
  KwType,
  KwProc,
  KwIt,

  // Directives
  DirRun,
  DirLoad,
  DirInline,
  DirNoInline,

  // Multi-char operators
  ColonColon,  // ::
  Arrow,       // ->
  DotDot,      // ..
  Ellipsis,    // --- (uninitialized)
  DynamicArray, // [..]
  EqEq,
  NotEq,
  LessEq,
  GreaterEq,
  AndAnd,
  OrOr,
  LeftShift,
  RightShift,

  // Single-char
  Semicolon,
  Comma,
  Dot,
  Colon,
  Question,
  Exclaim,
  Eq,
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  Less,
  Greater,
  Amp,
  Pipe,
  Caret,
  Tilde,
  At,
  Dollar,

  // Brackets
  LParen,
  RParen,
  LBracket,
  RBracket,
  LBrace,
  RBrace,
};

struct Token {
  TokenKind kind = TokenKind::EndOfFile;
  SourceLoc loc;
  std::string_view text;  // view into source

  bool is(TokenKind k) const { return kind == k; }
  bool is_one_of(TokenKind a, TokenKind b) const {
    return kind == a || kind == b;
  }
  template <typename... Ts>
  bool is_one_of(TokenKind a, TokenKind b, Ts... rest) const {
    return kind == a || is_one_of(b, rest...);
  }
};

class Lexer {
 public:
  explicit Lexer(std::string_view source);

  Token next();
  Token peek();
  const Token& current() const { return current_; }
  bool at_eof() const { return current_.kind == TokenKind::EndOfFile; }
  std::string_view source() const { return source_; }

 private:
  char peek_char() const;
  char peek_char(size_t offset) const;
  char consume();
  bool consume_if(char c);
  void skip_line_comment();
  void skip_block_comment();  // supports nesting
  Token make_token(TokenKind kind);
  Token lex_number();
  Token lex_string();
  Token lex_identifier_or_keyword();
  Token lex_directive();

  std::string_view source_;
  size_t pos_ = 0;
  SourceLoc loc_;
  Token current_;
};

}  // namespace jai
