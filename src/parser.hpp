#pragma once

#include "ast.hpp"
#include "lexer.hpp"
#include <memory>
#include <string>
#include <vector>

namespace jai {

class Parser {
 public:
  explicit Parser(std::string source, std::string path = "");

  std::unique_ptr<File> parse_file();
  bool has_errors() const { return !errors_.empty(); }
  const std::vector<std::string>& errors() const { return errors_; }

 private:
  void advance();
  bool expect(TokenKind k);
  bool check(TokenKind k) const;
  bool check_one_of(TokenKind a, TokenKind b) const;
  void add_error(const std::string& msg);

  std::unique_ptr<Decl> parse_declaration();
  std::unique_ptr<Decl> parse_var_decl();
  std::unique_ptr<Decl> parse_var_decl_after_name(std::string name);
  std::unique_ptr<Decl> parse_proc_decl();
  std::unique_ptr<Decl> parse_proc_decl_after_name(std::string name);
  std::unique_ptr<Decl> parse_struct_decl();
  std::unique_ptr<Decl> parse_struct_decl_after_name(std::string name);
  std::unique_ptr<Decl> parse_enum_decl();
  std::unique_ptr<Decl> parse_enum_decl_after_name(std::string name);
  std::unique_ptr<Decl> parse_directive();
  std::unique_ptr<Decl> parse_extern_directive();

  std::unique_ptr<Stmt> parse_statement();
  std::unique_ptr<Stmt> parse_block();
  std::unique_ptr<Stmt> parse_if();
  std::unique_ptr<Stmt> parse_for();
  std::unique_ptr<Stmt> parse_loop();
  std::unique_ptr<Stmt> parse_return();
  std::unique_ptr<Stmt> parse_defer();

  std::unique_ptr<Expr> parse_expr(int prec = 0, bool allow_init_list = true);
  std::unique_ptr<Expr> parse_primary(bool allow_init_list = true);
  std::unique_ptr<Expr> parse_postfix(std::unique_ptr<Expr> lhs);
  int precedence(TokenKind op);

  std::unique_ptr<TypeExpr> parse_type_expr();
  std::unique_ptr<TypeExpr> parse_type_expr_primary();

  std::vector<Param> parse_param_list();
  std::vector<std::string> parse_capture_list();
  std::vector<StructMember> parse_struct_members();
  std::vector<EnumMember> parse_enum_members();
  std::unique_ptr<Stmt> parse_var_decl_as_stmt();

  std::string source_;
  std::string path_;
  Lexer lexer_;
  Token current_;
  std::vector<std::string> errors_;
};

}  // namespace jai
