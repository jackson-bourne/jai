#pragma once

#include "lexer.hpp"
#include <memory>
#include <string>
#include <vector>

namespace jai {

struct SourceLoc;

// Forward declarations
struct TypeExpr;
struct Expr;
struct Stmt;
struct Decl;
struct File;

// Type expressions (syntax only; semantic pass resolves to types)
struct TypeExpr {
  SourceLoc loc;
  enum class Kind {
    Ident,       // T, int, float, MyStruct
    Pointer,     // *T
    OwnedPointer, // !*T
    ArrayFixed,  // [N] T
    ArrayDynamic, // [..] T
    Optional,    // T? (stub)
  };
  Kind kind = Kind::Ident;
  std::string name;  // for Ident
  bool type_param = false;  // $T
  std::unique_ptr<TypeExpr> element_type;  // *T, [N]T, [..]T
  std::unique_ptr<Expr> count_expr;  // [N] when N is expression
  int fixed_count = 0;  // [N] when N is literal
};

// Expressions
struct Expr {
  SourceLoc loc;
  enum class Kind {
    Invalid,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    BoolLiteral,
    NullLiteral,
    Ident,
    Binary,
    Unary,
    Call,
    Index,
    Member,
    Cast,
    Run,  // #run expr() - compile-time
    InitList,  // { a, b, c } for array/struct init
  };
  Kind kind = Kind::Invalid;
  int64_t int_val = 0;
  double float_val = 0;
  std::string string_val;
  bool bool_val = false;

  std::string ident;
  bool type_param_ident = false;  // $T in param list

  std::unique_ptr<Expr> lhs;
  std::unique_ptr<Expr> rhs;
  TokenKind op = TokenKind::EndOfFile;

  std::unique_ptr<Expr> operand;  // unary, cast
  std::vector<std::unique_ptr<Expr>> args;  // call
  std::unique_ptr<Expr> index_expr;  // a[i]
  std::string member_name;  // .member

  std::unique_ptr<TypeExpr> cast_type;  // cast(T) x
  std::unique_ptr<Expr> run_expr;  // #run f()

  std::vector<std::unique_ptr<Expr>> init_list;
};

// Statements
struct Stmt {
  SourceLoc loc;
  enum class Kind {
    Invalid,
    Block,
    Expr,
    VarDecl,
    If,
    ForRange,   // for i : 0..n-1
    ForIter,    // for container { }
    While,
    Return,
    Defer,
  };
  Kind kind = Kind::Invalid;
  std::vector<std::unique_ptr<Stmt>> block_body;
  std::unique_ptr<Expr> expr;

  std::string var_name;
  std::unique_ptr<TypeExpr> var_type;
  std::unique_ptr<Expr> var_init;
  bool var_infer_type = false;  // :=
  bool var_no_init = false;    // = ---

  std::unique_ptr<Expr> if_cond;
  std::unique_ptr<Stmt> if_then;
  std::unique_ptr<Stmt> if_else;

  std::string for_var_name;
  std::unique_ptr<Expr> for_range_start;
  std::unique_ptr<Expr> for_range_end;
  std::unique_ptr<Expr> for_iter_expr;
  std::unique_ptr<Stmt> for_body;

  std::unique_ptr<Expr> while_cond;
  std::unique_ptr<Stmt> while_body;

  std::unique_ptr<Expr> return_expr;
  std::unique_ptr<Stmt> defer_body;
};

// Parameter: name, type, optional default
struct Param {
  SourceLoc loc;
  std::string name;
  std::unique_ptr<TypeExpr> type;
  bool type_param = false;  // $T
};

// Function/procedure
struct ProcDecl {
  SourceLoc loc;
  std::string name;
  std::vector<Param> params;
  std::unique_ptr<TypeExpr> return_type;
  std::vector<std::string> capture_list;  // [m, x, y]
  std::unique_ptr<Stmt> body;
  bool is_anonymous = false;
  bool inline_requested = false;
  bool no_inline_requested = false;
};

// Struct member
struct StructMember {
  SourceLoc loc;
  std::string name;
  std::unique_ptr<TypeExpr> type;
  std::unique_ptr<Expr> default_init;
  bool no_init = false;  // = ---
};

// Enum member
struct EnumMember {
  SourceLoc loc;
  std::string name;
  std::unique_ptr<Expr> value;  // optional explicit value
};

// Declarations
struct Decl {
  SourceLoc loc;
  enum class Kind {
    Invalid,
    Var,
    Const,  // name :: expr;
    Proc,
    Struct,
    Enum,
    DirectiveRun,
    DirectiveLoad,
    DirectiveInline,
    DirectiveNoInline,
    DirectiveExtern,
  };
  Kind kind = Kind::Invalid;

  std::string var_name;
  std::unique_ptr<TypeExpr> var_type;
  std::unique_ptr<Expr> var_init;
  bool var_infer_type = false;
  bool var_no_init = false;

  std::unique_ptr<ProcDecl> proc;
  std::string struct_name;
  bool struct_soa = false;
  std::vector<StructMember> struct_members;
  std::string enum_name;
  std::unique_ptr<TypeExpr> enum_backing_type;
  std::vector<EnumMember> enum_members;

  std::unique_ptr<Expr> directive_run_expr;
  std::string directive_load_path;
  std::string directive_inline_proc;
  bool directive_no_inline = false;
};

struct File {
  std::string path;
  std::string source;
  std::vector<std::unique_ptr<Decl>> declarations;
};

// Helpers
inline SourceLoc loc_from_token(const Token& t) {
  SourceLoc loc;
  loc.line = t.loc.line;
  loc.column = t.loc.column;
  loc.offset = t.loc.offset;
  return loc;
}

}  // namespace jai
