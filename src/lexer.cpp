#include "lexer.hpp"
#include <cctype>
#include <unordered_map>

namespace jai {

namespace {

const std::unordered_map<std::string_view, TokenKind> keywords = {
    {"struct", TokenKind::KwStruct},   {"enum", TokenKind::KwEnum},
    {"for", TokenKind::KwFor},         {"loop", TokenKind::KwLoop},
    {"if", TokenKind::KwIf},
    {"else", TokenKind::KwElse},
    {"return", TokenKind::KwReturn},   {"inline", TokenKind::KwInline},
    {"no_inline", TokenKind::KwNoInline}, {"defer", TokenKind::KwDefer},
    {"new", TokenKind::KwNew},         {"delete", TokenKind::KwDelete},
    {"cast", TokenKind::KwCast},       {"then", TokenKind::KwThen},
    {"true", TokenKind::KwTrue},       {"false", TokenKind::KwFalse},
    {"null", TokenKind::KwNull},       {"SOA", TokenKind::KwSOA},
    {"Any", TokenKind::KwAny},        {"type", TokenKind::KwType},
    {"proc", TokenKind::KwProc},       {"it", TokenKind::KwIt},
};

const std::unordered_map<std::string_view, TokenKind> directives = {
    {"run", TokenKind::DirRun},
    {"load", TokenKind::DirLoad},
    {"inline", TokenKind::DirInline},
    {"no_inline", TokenKind::DirNoInline},
};

}  // namespace

Lexer::Lexer(std::string_view source) : source_(source) {
  current_.loc = loc_;
  // First token is obtained by first call to next()
}

char Lexer::peek_char() const {
  return peek_char(0);
}

char Lexer::peek_char(size_t offset) const {
  if (pos_ + offset >= source_.size()) return '\0';
  return source_[pos_ + offset];
}

char Lexer::consume() {
  if (pos_ >= source_.size()) return '\0';
  char c = source_[pos_++];
  if (c == '\n') {
    loc_.line++;
    loc_.column = 1;
  } else {
    loc_.column++;
  }
  loc_.offset = static_cast<uint32_t>(pos_);
  return c;
}

bool Lexer::consume_if(char c) {
  if (peek_char() == c) {
    consume();
    return true;
  }
  return false;
}

void Lexer::skip_line_comment() {
  while (peek_char() && peek_char() != '\n') consume();
}

void Lexer::skip_block_comment() {
  int depth = 1;
  while (depth > 0 && peek_char()) {
    if (consume_if('/') && peek_char() == '*') {
      consume();
      depth++;
    } else if (consume_if('*') && peek_char() == '/') {
      consume();
      depth--;
    } else {
      consume();
    }
  }
}

Token Lexer::make_token(TokenKind kind) {
  Token t;
  t.kind = kind;
  t.loc = loc_;
  t.text = source_.substr(loc_.offset, static_cast<size_t>(loc_.column) + (pos_ - loc_.offset) - 1);
  return t;
}

Token Lexer::lex_number() {
  SourceLoc start = loc_;
  size_t start_pos = pos_;
  bool is_float = false;
  while (std::isdigit(static_cast<unsigned char>(peek_char()))) consume();
  if (peek_char() == '.' && std::isdigit(static_cast<unsigned char>(peek_char(1)))) {
    consume();
    is_float = true;
    while (std::isdigit(static_cast<unsigned char>(peek_char()))) consume();
  }
  if ((peek_char() == 'e' || peek_char() == 'E') && !is_float) {
    consume();
    if (peek_char() == '+' || peek_char() == '-') consume();
    while (std::isdigit(static_cast<unsigned char>(peek_char()))) consume();
    is_float = true;
  }
  std::string_view text = source_.substr(start_pos, pos_ - start_pos);
  Token t;
  t.kind = is_float ? TokenKind::FloatLiteral : TokenKind::IntLiteral;
  t.loc = start;
  t.text = text;
  return t;
}

Token Lexer::lex_string() {
  SourceLoc start = loc_;
  size_t start_pos = pos_;
  char quote = consume();  // " or '
  while (peek_char() && peek_char() != quote) {
    if (peek_char() == '\\') consume(), consume();
    else consume();
  }
  if (peek_char() == quote) consume();
  Token t;
  t.kind = TokenKind::StringLiteral;
  t.loc = start;
  t.text = source_.substr(start_pos, pos_ - start_pos);
  return t;
}

Token Lexer::lex_identifier_or_keyword() {
  SourceLoc start = loc_;
  size_t start_pos = pos_;
  char p;
  while ((p = peek_char()) && (((p >= 'a' && p <= 'z') || (p >= 'A' && p <= 'Z') ||
                                (p >= '0' && p <= '9') || p == '_'))) {
    consume();
  }
  std::string_view text = source_.substr(start_pos, pos_ - start_pos);
  auto it = keywords.find(text);
  Token t;
  t.kind = it != keywords.end() ? it->second : TokenKind::Identifier;
  t.loc = start;
  t.text = text;
  return t;
}

Token Lexer::lex_directive() {
  consume();  // #
  SourceLoc start = loc_;
  size_t start_pos = pos_;
  char p;
  while ((p = peek_char()) && (((p >= 'a' && p <= 'z') || (p >= 'A' && p <= 'Z') ||
                                (p >= '0' && p <= '9') || p == '_'))) {
    consume();
  }
  std::string_view name = source_.substr(start_pos, pos_ - start_pos);
  auto it = directives.find(name);
  Token t;
  t.kind = it != directives.end() ? it->second : TokenKind::Invalid;
  t.loc = start;
  t.text = source_.substr(start_pos - 1, pos_ - start_pos + 1);
  return t;
}

Token Lexer::next() {
  current_.loc.offset = static_cast<uint32_t>(pos_);
  current_.loc.line = loc_.line;
  current_.loc.column = loc_.column;

  while (true) {
    size_t token_start = pos_;
    current_.loc.offset = static_cast<uint32_t>(pos_);
    current_.loc.line = loc_.line;
    current_.loc.column = loc_.column;

    char c = peek_char();
    if (!c) {
      current_.kind = TokenKind::EndOfFile;
      current_.loc = loc_;
      current_.text = {};
      return current_;
    }

    if (c == '/') {
      if (peek_char(1) == '/') {
        consume(), consume();
        skip_line_comment();
        continue;
      }
      if (peek_char(1) == '*') {
        consume(), consume();
        skip_block_comment();
        continue;
      }
    }

    if (c == '#') {
      current_ = lex_directive();
      return current_;
    }

    if (std::isspace(static_cast<unsigned char>(c))) {
      consume();
      continue;
    }

    if (std::isdigit(static_cast<unsigned char>(c))) {
      current_ = lex_number();
      return current_;
    }

    if (c == '"' || c == '\'') {
      current_ = lex_string();
      return current_;
    }

    if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
      current_ = lex_identifier_or_keyword();
      return current_;
    }

    consume();
    current_.loc.offset = static_cast<uint32_t>(token_start);
    current_.text = source_.substr(token_start, 1);

    if (c == '-') {
      if (peek_char() == '>') {
        consume();
        current_.kind = TokenKind::Arrow;
        current_.text = source_.substr(token_start, 2);
        return current_;
      }
      if (peek_char() == '-' && peek_char(1) == '-') {
        consume(), consume();
        current_.kind = TokenKind::Ellipsis;
        current_.text = source_.substr(token_start, 3);
        return current_;
      }
      current_.kind = TokenKind::Minus;
      return current_;
    }

    switch (c) {
      case ';': current_.kind = TokenKind::Semicolon; return current_;
      case ',': current_.kind = TokenKind::Comma; return current_;
      case '.':
        if (peek_char() == '.') {
          consume();
          current_.kind = TokenKind::DotDot;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Dot;
        }
        return current_;
      case '?': current_.kind = TokenKind::Question; return current_;
      case '+': current_.kind = TokenKind::Plus; return current_;
      case '*': current_.kind = TokenKind::Star; return current_;
      case '/': current_.kind = TokenKind::Slash; return current_;
      case '%': current_.kind = TokenKind::Percent; return current_;
      case '^': current_.kind = TokenKind::Caret; return current_;
      case '~': current_.kind = TokenKind::Tilde; return current_;
      case '@': current_.kind = TokenKind::At; return current_;
      case '$': current_.kind = TokenKind::Dollar; return current_;
      case '(': current_.kind = TokenKind::LParen; return current_;
      case ')': current_.kind = TokenKind::RParen; return current_;
      case '[':
        if (peek_char() == '.' && peek_char(1) == '.' && peek_char(2) == ']') {
          consume(), consume(), consume();
          current_.kind = TokenKind::DynamicArray;
          current_.text = source_.substr(token_start, 4);
          return current_;
        }
        current_.kind = TokenKind::LBracket;
        return current_;
      case ']': current_.kind = TokenKind::RBracket; return current_;
      case '{': current_.kind = TokenKind::LBrace; return current_;
      case '}': current_.kind = TokenKind::RBrace; return current_;
      case '<':
        if (peek_char() == '=') {
          consume();
          current_.kind = TokenKind::LessEq;
          current_.text = source_.substr(token_start, 2);
        } else if (peek_char() == '<') {
          consume(), consume();
          current_.kind = TokenKind::LeftShift;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Less;
        }
        return current_;
      case '>':
        if (peek_char() == '=') {
          consume();
          current_.kind = TokenKind::GreaterEq;
          current_.text = source_.substr(token_start, 2);
        } else if (peek_char() == '>') {
          consume(), consume();
          current_.kind = TokenKind::RightShift;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Greater;
        }
        return current_;
      case '=':
        if (peek_char() == '=') {
          consume();
          current_.kind = TokenKind::EqEq;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Eq;
        }
        return current_;
      case '!':
        if (peek_char() == '=') {
          consume();
          current_.kind = TokenKind::NotEq;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Exclaim;
        }
        return current_;
      case '&':
        if (peek_char() == '&') {
          consume();
          current_.kind = TokenKind::AndAnd;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Amp;
        }
        return current_;
      case '|':
        if (peek_char() == '|') {
          consume();
          current_.kind = TokenKind::OrOr;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Pipe;
        }
        return current_;
      case ':':
        if (peek_char() == ':') {
          consume();
          current_.kind = TokenKind::ColonColon;
          current_.text = source_.substr(token_start, 2);
        } else {
          current_.kind = TokenKind::Colon;
        }
        return current_;
      case '-':
        current_.kind = TokenKind::Minus;
        return current_;
      default:
        current_.kind = TokenKind::Invalid;
        return current_;
    }
  }
}

Token Lexer::peek() {
  size_t save_pos = pos_;
  SourceLoc save_loc = loc_;
  Token t = next();
  pos_ = save_pos;
  loc_ = save_loc;
  return t;
}

}  // namespace jai
