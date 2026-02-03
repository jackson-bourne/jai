#include "parser.hpp"
#include <sstream>

namespace jai {

Parser::Parser(std::string source, std::string path)
    : source_(std::move(source)), path_(std::move(path)), lexer_(source_) {
  advance();
}

void Parser::advance() {
  current_ = lexer_.next();
}

bool Parser::expect(TokenKind k) {
  if (current_.kind == k) {
    advance();
    return true;
  }
  return false;
}

bool Parser::check(TokenKind k) const {
  return current_.kind == k;
}

bool Parser::check_one_of(TokenKind a, TokenKind b) const {
  return current_.kind == a || current_.kind == b;
}

void Parser::add_error(const std::string& msg) {
  std::ostringstream os;
  os << path_ << ":" << current_.loc.line << ":" << current_.loc.column << ": "
     << msg;
  errors_.push_back(os.str());
}

int Parser::precedence(TokenKind op) {
  switch (op) {
    case TokenKind::Eq: return 1;   // assignment (lowest)
    case TokenKind::OrOr: return 2;
    case TokenKind::AndAnd: return 3;
    case TokenKind::EqEq:
    case TokenKind::NotEq:
    case TokenKind::Less:
    case TokenKind::LessEq:
    case TokenKind::Greater:
    case TokenKind::GreaterEq: return 4;
    case TokenKind::Pipe:
    case TokenKind::Caret:
    case TokenKind::Amp: return 5;
    case TokenKind::LeftShift:
    case TokenKind::RightShift: return 6;
    case TokenKind::Plus:
    case TokenKind::Minus: return 7;
    case TokenKind::Star:
    case TokenKind::Slash:
    case TokenKind::Percent: return 8;
    default: return 0;
  }
}

std::unique_ptr<File> Parser::parse_file() {
  auto file = std::make_unique<File>();
  file->path = path_;
  file->source = source_;

  while (!check(TokenKind::EndOfFile)) {
    while (check(TokenKind::Semicolon)) advance();
    if (check(TokenKind::EndOfFile)) break;
    if (check(TokenKind::DirRun)) {
      auto d = parse_directive();
      if (d) file->declarations.push_back(std::move(d));
      continue;
    }
    if (check(TokenKind::DirLoad)) {
      auto d = parse_directive();
      if (d) file->declarations.push_back(std::move(d));
      continue;
    }
    if (check(TokenKind::DirInline) || check(TokenKind::DirNoInline)) {
      auto d = parse_directive();
      if (d) file->declarations.push_back(std::move(d));
      continue;
    }
    if (check(TokenKind::At)) {
      advance();
      if (check(TokenKind::Identifier) && current_.text == "extern") {
        advance();
        auto d = parse_extern_directive();
        if (d) file->declarations.push_back(std::move(d));
      } else {
        add_error("expected @extern");
        if (check(TokenKind::Identifier)) advance();
      }
      continue;
    }
    auto decl = parse_declaration();
    if (decl) file->declarations.push_back(std::move(decl));
    else if (!has_errors()) break;  // EOF or recovery
  }
  return file;
}

std::unique_ptr<Decl> Parser::parse_declaration() {
  if (check(TokenKind::EndOfFile)) return nullptr;
  if (!check(TokenKind::Identifier)) {
    add_error("expected identifier or declaration keyword");
    advance();
    return nullptr;
  }
  std::string name(current_.text);
  advance();

  if (check(TokenKind::ColonColon)) {
    advance();
    if (check(TokenKind::LParen)) return parse_proc_decl_after_name(name);
    if (check(TokenKind::KwStruct)) return parse_struct_decl_after_name(name);
    if (check(TokenKind::KwEnum)) return parse_enum_decl_after_name(name);
    // Constant: name :: expr;
    auto decl = std::make_unique<Decl>();
    decl->kind = Decl::Kind::Const;
    decl->loc = loc_from_token(current_);
    decl->var_name = std::move(name);
    decl->var_init = parse_expr();
    if (!decl->var_init) add_error("expected constant value after ::");
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after constant");
    return decl;
  }

  if (check(TokenKind::Colon) || check(TokenKind::Eq)) {
    return parse_var_decl_after_name(name);
  }

  add_error("expected :: or : after identifier in declaration");
  return nullptr;
}

std::unique_ptr<Decl> Parser::parse_var_decl_after_name(std::string name) {
  auto decl = std::make_unique<Decl>();
  decl->kind = Decl::Kind::Var;
  decl->loc = loc_from_token(current_);
  decl->var_name = std::move(name);

  if (expect(TokenKind::Colon)) {
    if (check(TokenKind::Eq)) {
      advance();
      decl->var_infer_type = true;
      decl->var_init = parse_expr();
      if (!decl->var_init) add_error("expected initializer for :=");
    } else {
      decl->var_type = parse_type_expr();
      if (!decl->var_type) {
        add_error("expected type after :");
        return nullptr;
      }
      if (check(TokenKind::Eq)) {
        advance();
        if (check(TokenKind::Ellipsis)) {
          advance();
          decl->var_no_init = true;
        } else {
          decl->var_init = parse_expr();
          if (!decl->var_init) add_error("expected initializer");
        }
      }
    }
  } else if (expect(TokenKind::Eq)) {
    if (check(TokenKind::Ellipsis)) {
      advance();
      decl->var_no_init = true;
      add_error(":= required for type inference with value");
    } else {
      decl->var_infer_type = true;
      decl->var_init = parse_expr();
      if (!decl->var_init) add_error("expected initializer for :=");
    }
  }

  if (!expect(TokenKind::Semicolon)) add_error("expected ;");
  return decl;
}

std::unique_ptr<Decl> Parser::parse_var_decl() {
  if (!check(TokenKind::Identifier)) return nullptr;
  std::string name(current_.text);
  advance();
  return parse_var_decl_after_name(name);
}

std::unique_ptr<Decl> Parser::parse_proc_decl_after_name(std::string name) {
  auto proc = std::make_unique<ProcDecl>();
  proc->loc = loc_from_token(current_);
  proc->name = std::move(name);

  if (!expect(TokenKind::LParen)) {
    add_error("expected ( for parameter list");
    return nullptr;
  }
  if (!check(TokenKind::RParen)) {
    proc->params = parse_param_list();
  }
  if (!expect(TokenKind::RParen)) add_error("expected )");

  // Arrow (->) for return type; accept Minus+Greater if lexer split them (e.g. across newline)
  if (check(TokenKind::Arrow)) {
    advance();
    proc->return_type = parse_type_expr();
    if (!proc->return_type) proc->return_type = std::make_unique<TypeExpr>();
  } else if (check(TokenKind::Minus) && lexer_.peek().kind == TokenKind::Greater) {
    advance();
    advance();
    proc->return_type = parse_type_expr();
    if (!proc->return_type) proc->return_type = std::make_unique<TypeExpr>();
  }

  if (check(TokenKind::LBracket)) {
    advance();
    while (check(TokenKind::Identifier)) {
      proc->capture_list.push_back(std::string(current_.text));
      advance();
      if (!expect(TokenKind::Comma)) break;
    }
    if (!expect(TokenKind::RBracket)) add_error("expected ]");
  }

  if (check(TokenKind::KwInline)) {
    advance();
    proc->inline_requested = true;
  }
  if (check(TokenKind::KwNoInline)) {
    advance();
    proc->no_inline_requested = true;
  }

  proc->body = parse_block();
  if (!proc->body) add_error("expected { } body");
  expect(TokenKind::Semicolon);  // optional per spec (most examples omit it)

  auto decl = std::make_unique<Decl>();
  decl->kind = Decl::Kind::Proc;
  decl->loc = proc->loc;
  decl->proc = std::move(proc);
  return decl;
}

std::unique_ptr<Decl> Parser::parse_proc_decl() {
  if (!check(TokenKind::Identifier)) return nullptr;
  std::string name(current_.text);
  advance();
  if (!check(TokenKind::ColonColon)) return nullptr;
  advance();
  return parse_proc_decl_after_name(name);
}

std::unique_ptr<Decl> Parser::parse_struct_decl() {
  if (!expect(TokenKind::KwStruct)) return nullptr;
  if (!check(TokenKind::Identifier)) {
    add_error("expected struct name");
    return nullptr;
  }
  std::string name(current_.text);
  advance();
  return parse_struct_decl_after_name(name);
}

std::unique_ptr<Decl> Parser::parse_struct_decl_after_name(std::string name) {
  if (!expect(TokenKind::KwStruct)) return nullptr;
  auto decl = std::make_unique<Decl>();
  decl->kind = Decl::Kind::Struct;
  decl->loc = loc_from_token(current_);
  decl->struct_name = std::move(name);
  if (check(TokenKind::KwSOA)) {
    advance();
    decl->struct_soa = true;
  }
  if (!expect(TokenKind::LBrace)) add_error("expected {");
  decl->struct_members = parse_struct_members();
  if (!expect(TokenKind::RBrace)) add_error("expected }");
  expect(TokenKind::Semicolon);  // optional per spec
  return decl;
}

std::unique_ptr<Decl> Parser::parse_enum_decl() {
  if (!expect(TokenKind::KwEnum)) return nullptr;
  if (!check(TokenKind::Identifier)) {
    add_error("expected enum name");
    return nullptr;
  }
  std::string name(current_.text);
  advance();
  return parse_enum_decl_after_name(name);
}

std::unique_ptr<Decl> Parser::parse_enum_decl_after_name(std::string name) {
  if (!expect(TokenKind::KwEnum)) return nullptr;
  auto decl = std::make_unique<Decl>();
  decl->kind = Decl::Kind::Enum;
  decl->loc = loc_from_token(current_);
  decl->enum_name = std::move(name);
  if (check(TokenKind::Identifier) &&
      (current_.text == "u8" || current_.text == "u16" || current_.text == "u32" ||
       current_.text == "u64" || current_.text == "int" || current_.text == "s8" ||
       current_.text == "s16" || current_.text == "s32" || current_.text == "s64")) {
    decl->enum_backing_type = parse_type_expr_primary();
  }
  if (!expect(TokenKind::LBrace)) add_error("expected {");
  decl->enum_members = parse_enum_members();
  if (!expect(TokenKind::RBrace)) add_error("expected }");
  expect(TokenKind::Semicolon);  // optional per spec
  return decl;
}

std::unique_ptr<Decl> Parser::parse_directive() {
  SourceLoc loc = loc_from_token(current_);
  if (check(TokenKind::DirRun)) {
    advance();
    auto decl = std::make_unique<Decl>();
    decl->kind = Decl::Kind::DirectiveRun;
    decl->loc = loc;
    decl->directive_run_expr = parse_expr();
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after #run");
    return decl;
  }
  if (check(TokenKind::DirLoad)) {
    advance();
    if (!check(TokenKind::StringLiteral)) {
      add_error("expected string path after #load");
      return nullptr;
    }
    auto decl = std::make_unique<Decl>();
    decl->kind = Decl::Kind::DirectiveLoad;
    decl->loc = loc;
    decl->directive_load_path = std::string(current_.text);
    advance();
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after #load");
    return decl;
  }
  if (check(TokenKind::DirInline)) {
    advance();
    if (!check(TokenKind::Identifier)) {
      add_error("expected procedure name after #inline");
      return nullptr;
    }
    auto decl = std::make_unique<Decl>();
    decl->kind = Decl::Kind::DirectiveInline;
    decl->loc = loc;
    decl->directive_inline_proc = std::string(current_.text);
    advance();
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after #inline");
    return decl;
  }
  if (check(TokenKind::DirNoInline)) {
    advance();
    if (!check(TokenKind::Identifier)) {
      add_error("expected procedure name after #no_inline");
      return nullptr;
    }
    auto decl = std::make_unique<Decl>();
    decl->kind = Decl::Kind::DirectiveNoInline;
    decl->loc = loc;
    decl->directive_inline_proc = std::string(current_.text);
    decl->directive_no_inline = true;
    advance();
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after #no_inline");
    return decl;
  }
  return nullptr;
}

std::unique_ptr<Decl> Parser::parse_extern_directive() {
  SourceLoc loc = loc_from_token(current_);
  if (!check(TokenKind::Identifier)) {
    add_error("expected name after @extern");
    return nullptr;
  }
  std::string name(current_.text);
  advance();
  auto decl = std::make_unique<Decl>();
  decl->kind = Decl::Kind::DirectiveExtern;
  decl->loc = loc;
  if (check(TokenKind::ColonColon)) {
    // Function: @extern Name :: (params) -> return_type;
    advance();
    if (!expect(TokenKind::LParen)) {
      add_error("expected ( for parameter list after @extern name ::");
      return nullptr;
    }
    auto proc = std::make_unique<ProcDecl>();
    proc->loc = loc;
    proc->name = std::move(name);
    if (!check(TokenKind::RParen)) proc->params = parse_param_list();
    if (!expect(TokenKind::RParen)) add_error("expected )");
    if (check(TokenKind::Arrow)) {
      advance();
      proc->return_type = parse_type_expr();
      if (!proc->return_type) proc->return_type = std::make_unique<TypeExpr>();
    } else if (check(TokenKind::Minus) && lexer_.peek().kind == TokenKind::Greater) {
      advance();
      advance();
      proc->return_type = parse_type_expr();
      if (!proc->return_type) proc->return_type = std::make_unique<TypeExpr>();
    }
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after @extern declaration");
    decl->proc = std::move(proc);
    return decl;
  }
  if (check(TokenKind::Colon)) {
    // Variable: @extern name: Type;
    advance();
    decl->var_name = std::move(name);
    decl->var_type = parse_type_expr();
    if (!decl->var_type) add_error("expected type after @extern name:");
    if (!expect(TokenKind::Semicolon)) add_error("expected ; after @extern variable");
    return decl;
  }
  add_error("expected :: for function or : for variable after @extern name");
  return nullptr;
}

std::vector<Param> Parser::parse_param_list() {
  std::vector<Param> params;
  while (true) {
    if (!check(TokenKind::Identifier) && !check(TokenKind::Dollar)) break;
    Param p;
    p.loc = loc_from_token(current_);
    if (check(TokenKind::Dollar)) {
      advance();
      if (!check(TokenKind::Identifier)) {
        add_error("expected type parameter name after $");
        break;
      }
      p.name = std::string(current_.text);
      p.type_param = true;
      advance();
    } else {
      p.name = std::string(current_.text);
      advance();
    }
    if (!p.type_param && expect(TokenKind::Colon)) {
      p.type = parse_type_expr();
    }
    params.push_back(std::move(p));
    if (!expect(TokenKind::Comma)) break;
  }
  return params;
}

std::vector<std::string> Parser::parse_capture_list() {
  std::vector<std::string> list;
  if (!expect(TokenKind::LBracket)) return list;
  while (check(TokenKind::Identifier)) {
    list.push_back(std::string(current_.text));
    advance();
    if (!expect(TokenKind::Comma)) break;
  }
  expect(TokenKind::RBracket);
  return list;
}

std::vector<StructMember> Parser::parse_struct_members() {
  std::vector<StructMember> members;
  while (!check(TokenKind::RBrace) && !check(TokenKind::EndOfFile)) {
    if (!check(TokenKind::Identifier)) {
      add_error("expected member name");
      advance();
      continue;
    }
    StructMember m;
    m.loc = loc_from_token(current_);
    m.name = std::string(current_.text);
    advance();
    if (!expect(TokenKind::Colon)) continue;
    m.type = parse_type_expr();
    if (!m.type) continue;
    if (check(TokenKind::Eq)) {
      advance();
      if (check(TokenKind::Ellipsis)) {
        advance();
        m.no_init = true;
      } else {
        m.default_init = parse_expr();
      }
    }
    if (!expect(TokenKind::Semicolon)) add_error("expected ;");
    members.push_back(std::move(m));
  }
  return members;
}

std::vector<EnumMember> Parser::parse_enum_members() {
  std::vector<EnumMember> members;
  while (!check(TokenKind::RBrace) && !check(TokenKind::EndOfFile)) {
    if (!check(TokenKind::Identifier)) {
      add_error("expected enum member name");
      advance();
      continue;
    }
    EnumMember m;
    m.loc = loc_from_token(current_);
    m.name = std::string(current_.text);
    advance();
    if (check(TokenKind::Eq)) {
      advance();
      m.value = parse_expr();
    }
    if (!check(TokenKind::RBrace) && !expect(TokenKind::Comma))
      add_error("expected , or }");
    members.push_back(std::move(m));
  }
  return members;
}

std::unique_ptr<TypeExpr> Parser::parse_type_expr_primary() {
  if (check(TokenKind::DynamicArray)) {
    advance();
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::ArrayDynamic;
    t->loc = loc_from_token(current_);
    t->element_type = parse_type_expr();
    if (!t->element_type) return nullptr;
    return t;
  }
  if (check(TokenKind::LBracket)) {
    advance();
    if (check(TokenKind::DotDot)) {
      advance();
      auto t = std::make_unique<TypeExpr>();
      t->kind = TypeExpr::Kind::ArrayDynamic;
      t->loc = loc_from_token(current_);
      if (!expect(TokenKind::RBracket)) add_error("expected ]");
      t->element_type = parse_type_expr();
      if (!t->element_type) return nullptr;
      return t;
    }
    if (check(TokenKind::RBracket)) {
      // [] T - dynamic array
      advance();
      auto t = std::make_unique<TypeExpr>();
      t->kind = TypeExpr::Kind::ArrayDynamic;
      t->loc = loc_from_token(current_);
      t->element_type = parse_type_expr();
      if (!t->element_type) return nullptr;
      return t;
    }
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::ArrayFixed;
    t->loc = loc_from_token(current_);
    if (check(TokenKind::IntLiteral)) {
      t->fixed_count = static_cast<int>(std::stoll(std::string(current_.text)));
      advance();
    } else {
      t->count_expr = parse_expr();
    }
    if (!expect(TokenKind::RBracket)) add_error("expected ]");
    t->element_type = parse_type_expr();
    if (!t->element_type) return nullptr;
    return t;
  }
  if (check(TokenKind::Exclaim) && lexer_.peek().kind == TokenKind::Star) {
    advance(), advance();
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::OwnedPointer;
    t->loc = loc_from_token(current_);
    t->element_type = parse_type_expr_primary();
    if (!t->element_type) return nullptr;
    return t;
  }
  if (check(TokenKind::Star)) {
    advance();
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::Pointer;
    t->loc = loc_from_token(current_);
    t->element_type = parse_type_expr_primary();
    if (!t->element_type) return nullptr;
    return t;
  }
  if (check(TokenKind::Identifier)) {
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::Ident;
    t->loc = loc_from_token(current_);
    t->name = std::string(current_.text);
    advance();
    if (check(TokenKind::Question)) {
      advance();
      auto opt = std::make_unique<TypeExpr>();
      opt->kind = TypeExpr::Kind::Optional;
      opt->loc = t->loc;
      opt->name = t->name;
      return opt;
    }
    return t;
  }
  if (check(TokenKind::Dollar) && lexer_.peek().kind == TokenKind::Identifier) {
    advance();
    auto t = std::make_unique<TypeExpr>();
    t->kind = TypeExpr::Kind::Ident;
    t->loc = loc_from_token(current_);
    t->name = std::string(current_.text);
    t->type_param = true;
    advance();
    return t;
  }
  return nullptr;
}

std::unique_ptr<TypeExpr> Parser::parse_type_expr() {
  return parse_type_expr_primary();
}

std::unique_ptr<Expr> Parser::parse_primary(bool allow_init_list) {
  SourceLoc loc = loc_from_token(current_);
  if (check(TokenKind::IntLiteral)) {
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::IntLiteral;
    e->loc = loc;
    e->int_val = std::stoll(std::string(current_.text));
    advance();
    return e;
  }
  if (check(TokenKind::FloatLiteral)) {
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::FloatLiteral;
    e->loc = loc;
    e->float_val = std::stod(std::string(current_.text));
    advance();
    return e;
  }
  if (check(TokenKind::StringLiteral)) {
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::StringLiteral;
    e->loc = loc;
    e->string_val = std::string(current_.text);
    advance();
    return e;
  }
  if (check(TokenKind::KwTrue)) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::BoolLiteral;
    e->loc = loc;
    e->bool_val = true;
    return e;
  }
  if (check(TokenKind::KwFalse)) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::BoolLiteral;
    e->loc = loc;
    e->bool_val = false;
    return e;
  }
  if (check(TokenKind::KwNull)) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::NullLiteral;
    e->loc = loc;
    return e;
  }
  if (check(TokenKind::LParen)) {
    advance();
    if (check(TokenKind::RParen)) {
      advance();
      return nullptr;  // empty () - might be start of proc type or call
    }
    auto e = parse_expr();
    if (!expect(TokenKind::RParen)) add_error("expected )");
    return e;
  }
  if (check(TokenKind::KwCast)) {
    advance();
    if (!expect(TokenKind::LParen)) add_error("expected ( after cast");
    auto cast_type = parse_type_expr();
    if (!expect(TokenKind::RParen)) add_error("expected )");
    // Parse operand with high precedence so "cast(T) x * y" is (cast(T)x)*y not cast(T)(x*y)
    auto operand = parse_expr(8);
    if (!operand) add_error("expected expression after cast(type)");
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::Cast;
    e->loc = loc;
    e->cast_type = std::move(cast_type);
    e->operand = std::move(operand);
    return e;
  }
  if (check(TokenKind::Identifier) || check(TokenKind::KwIt)) {
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::Ident;
    e->loc = loc;
    e->ident = std::string(current_.text);
    advance();
    return e;
  }
  if (check(TokenKind::Dollar) && lexer_.peek().kind == TokenKind::Identifier) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::Ident;
    e->loc = loc;
    e->ident = std::string(current_.text);
    e->type_param_ident = true;
    advance();
    return e;
  }
  if (allow_init_list && check(TokenKind::LBrace)) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::InitList;
    e->loc = loc;
    while (!check(TokenKind::RBrace) && !check(TokenKind::EndOfFile)) {
      auto elem = parse_expr(0, true);
      if (elem) e->init_list.push_back(std::move(elem));
      if (!expect(TokenKind::Comma)) break;
    }
    expect(TokenKind::RBrace);
    return e;
  }
  return nullptr;
}

static bool is_member_name_token(TokenKind k) {
  if (k == TokenKind::Identifier) return true;
  switch (k) {
    case TokenKind::KwStruct: case TokenKind::KwEnum: case TokenKind::KwFor:
    case TokenKind::KwIf: case TokenKind::KwElse: case TokenKind::KwLoop:
    case TokenKind::KwReturn: case TokenKind::KwInline: case TokenKind::KwNoInline:
    case TokenKind::KwDefer: case TokenKind::KwNew: case TokenKind::KwDelete:
    case TokenKind::KwCast: case TokenKind::KwThen: case TokenKind::KwTrue:
    case TokenKind::KwFalse: case TokenKind::KwNull: case TokenKind::KwSOA:
    case TokenKind::KwAny: case TokenKind::KwType: case TokenKind::KwProc:
    case TokenKind::KwIt:
      return true;
    default: return false;
  }
}

std::unique_ptr<Expr> Parser::parse_postfix(std::unique_ptr<Expr> lhs) {
  while (true) {
    if (check(TokenKind::Dot)) {
      advance();  // consume '.'
      if (is_member_name_token(current_.kind)) {
        auto e = std::make_unique<Expr>();
        e->kind = Expr::Kind::Member;
        e->loc = loc_from_token(current_);
        e->lhs = std::move(lhs);
        e->member_name = std::string(current_.text);
        advance();
        lhs = std::move(e);
        continue;
      }
      add_error("expected member name after .");
      break;
    }
    if (check(TokenKind::LBracket)) {
      advance();
      auto e = std::make_unique<Expr>();
      e->kind = Expr::Kind::Index;
      e->loc = loc_from_token(current_);
      e->lhs = std::move(lhs);
      e->index_expr = parse_expr();
      if (!expect(TokenKind::RBracket)) add_error("expected ]");
      lhs = std::move(e);
      continue;
    }
    if (check(TokenKind::LParen)) {
      advance();
      auto e = std::make_unique<Expr>();
      e->kind = Expr::Kind::Call;
      e->loc = loc_from_token(current_);
      e->lhs = std::move(lhs);
      while (!check(TokenKind::RParen) && !check(TokenKind::EndOfFile)) {
        auto arg = parse_expr();
        if (arg) e->args.push_back(std::move(arg));
        if (!expect(TokenKind::Comma)) break;
      }
      expect(TokenKind::RParen);
      lhs = std::move(e);
      continue;
    }
    break;
  }
  return lhs;
}

std::unique_ptr<Expr> Parser::parse_expr(int prec, bool allow_init_list) {
  if (check(TokenKind::DirRun)) {
    advance();
    auto e = std::make_unique<Expr>();
    e->kind = Expr::Kind::Run;
    e->loc = loc_from_token(current_);
    e->run_expr = parse_expr(0, true);
    if (!e->run_expr) add_error("expected expression after #run");
    return e;
  }
  auto lhs = parse_primary(allow_init_list);
  if (!lhs) return nullptr;
  lhs = parse_postfix(std::move(lhs));

  while (true) {
    TokenKind op = current_.kind;
    int p = precedence(op);
    if (p == 0 || p <= prec) break;
    advance();
    auto rhs = parse_expr(p, allow_init_list);
    if (!rhs) {
      add_error("expected expression after operator");
      break;
    }
    auto bin = std::make_unique<Expr>();
    bin->kind = Expr::Kind::Binary;
    bin->loc = lhs->loc;
    bin->lhs = std::move(lhs);
    bin->op = op;
    bin->rhs = std::move(rhs);
    lhs = std::move(bin);
  }
  return lhs;
}

std::unique_ptr<Stmt> Parser::parse_block() {
  if (!expect(TokenKind::LBrace)) return nullptr;
  auto block = std::make_unique<Stmt>();
  block->kind = Stmt::Kind::Block;
  block->loc = loc_from_token(current_);
  while (!check(TokenKind::RBrace) && !check(TokenKind::EndOfFile)) {
    auto s = parse_statement();
    if (s) block->block_body.push_back(std::move(s));
    else if (check(TokenKind::Semicolon)) {
      advance();  // skip stray semicolon (e.g. after } in "}; ")
      continue;
    } else if (!has_errors()) break;
    else advance();  // skip past bad token to avoid infinite loop
  }
  expect(TokenKind::RBrace);
  return block;
}

std::unique_ptr<Stmt> Parser::parse_statement() {
  if (check(TokenKind::KwIf)) return parse_if();
  if (check(TokenKind::KwFor)) return parse_for();
  if (check(TokenKind::KwLoop)) return parse_loop();
  if (check(TokenKind::KwReturn)) return parse_return();
  if (check(TokenKind::KwDefer)) return parse_defer();
  if (check(TokenKind::LBrace)) return parse_block();

  // Declaration only when we see "id :" (e.g. z: int = 0). "id =" is assignment (z = 1).
  if (check(TokenKind::Identifier)) {
    Token peek_tok = lexer_.peek();
    if (peek_tok.kind == TokenKind::Colon) {
      return parse_var_decl_as_stmt();
    }
  }

  auto expr = parse_expr();
  if (expr) {
    auto s = std::make_unique<Stmt>();
    s->kind = Stmt::Kind::Expr;
    s->loc = expr->loc;
    s->expr = std::move(expr);
    if (!expect(TokenKind::Semicolon)) {
      if (!check(TokenKind::LBrace) && !check(TokenKind::RBrace))
        add_error("expected ;");
    }
    return s;
  }
  return nullptr;
}

std::unique_ptr<Stmt> Parser::parse_var_decl_as_stmt() {
  if (!check(TokenKind::Identifier)) return nullptr;
  std::string name(current_.text);
  advance();
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::VarDecl;
  s->loc = loc_from_token(current_);
  s->var_name = std::move(name);
  if (expect(TokenKind::Colon)) {
    s->var_type = parse_type_expr();
    if (check(TokenKind::Eq)) {
      advance();
      if (check(TokenKind::Ellipsis)) {
        advance();
        s->var_no_init = true;
      } else {
        s->var_init = parse_expr();
      }
    }
  } else if (expect(TokenKind::Eq)) {
    if (check(TokenKind::Ellipsis)) {
      advance();
      s->var_no_init = true;
    } else {
      s->var_infer_type = true;
      s->var_init = parse_expr();
    }
  }
  if (!expect(TokenKind::Semicolon)) add_error("expected ;");
  return s;
}

std::unique_ptr<Stmt> Parser::parse_if() {
  if (!expect(TokenKind::KwIf)) return nullptr;
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::If;
  s->loc = loc_from_token(current_);
  s->if_cond = parse_expr();
  if (check(TokenKind::KwThen)) advance();
  s->if_then = parse_statement();
  if (!s->if_then) add_error("expected then branch");
  if (check(TokenKind::KwElse)) {
    advance();
    s->if_else = parse_statement();
  }
  return s;
}

std::unique_ptr<Stmt> Parser::parse_for() {
  if (!expect(TokenKind::KwFor)) return nullptr;
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::ForRange;
  s->loc = loc_from_token(current_);
  if (check(TokenKind::Identifier)) {
    Token peek_tok = lexer_.peek();
    if (peek_tok.kind == TokenKind::Colon) {
      s->for_var_name = std::string(current_.text);
      advance();
      advance();  // consume :
      s->for_range_start = parse_expr();
      if (check(TokenKind::DotDot)) {
        advance();
        s->for_range_end = parse_expr();
      } else if (check(TokenKind::Dot) && lexer_.peek().kind == TokenKind::Dot) {
        advance();
        advance();
        s->for_range_end = parse_expr();
      }
      s->for_body = parse_statement();
      return s;
    }
  }
  s->for_iter_expr = parse_expr(0, false);  // don't parse { as init list
  if (s->for_iter_expr) {
    s->kind = Stmt::Kind::ForIter;
    if (check(TokenKind::LBrace)) {
      s->for_body = parse_block();
    } else {
      add_error("expected { for for-iter body");
    }
  }
  return s;
}

std::unique_ptr<Stmt> Parser::parse_loop() {
  if (!expect(TokenKind::KwLoop)) return nullptr;
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::Loop;
  s->loc = loc_from_token(current_);
  s->loop_cond = parse_expr();
  if (!s->loop_cond) add_error("expected condition after loop");
  s->loop_body = parse_statement();
  if (!s->loop_body) add_error("expected body after loop condition");
  return s;
}

std::unique_ptr<Stmt> Parser::parse_return() {
  if (!expect(TokenKind::KwReturn)) return nullptr;
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::Return;
  s->loc = loc_from_token(current_);
  if (!check(TokenKind::Semicolon) && !check(TokenKind::RBrace))
    s->return_expr = parse_expr();
  if (!expect(TokenKind::Semicolon)) add_error("expected ; after return");
  return s;
}

std::unique_ptr<Stmt> Parser::parse_defer() {
  if (!expect(TokenKind::KwDefer)) return nullptr;
  auto s = std::make_unique<Stmt>();
  s->kind = Stmt::Kind::Defer;
  s->loc = loc_from_token(current_);
  s->defer_body = parse_statement();
  if (!s->defer_body) add_error("expected statement after defer");
  return s;
}

}  // namespace jai
