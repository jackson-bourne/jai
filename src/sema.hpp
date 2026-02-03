#pragma once

#include "ast.hpp"
#include "types.hpp"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

namespace jai {

struct Symbol {
  enum class Kind { Var, Const, Proc, Struct, Enum };
  Kind kind = Kind::Var;
  std::string name;
  std::shared_ptr<Type> type;
  Decl* decl = nullptr;
  ProcDecl* proc_decl = nullptr;
  StructType* struct_type = nullptr;
  EnumType* enum_type = nullptr;
  std::optional<int64_t> const_int_value;  // for Const with integer value
};

class Scope {
 public:
  explicit Scope(Scope* parent = nullptr) : parent_(parent) {}
  void add(const std::string& name, Symbol sym);
  Symbol* find(const std::string& name);
  const Symbol* find(const std::string& name) const;
  Scope* parent() { return parent_; }

 private:
  Scope* parent_ = nullptr;
  std::unordered_map<std::string, Symbol> symbols_;
};

struct SemaContext {
  Scope* scope = nullptr;
  Scope file_scope_;  // kept so codegen can resolve file-level symbols (e.g. constants)
  File* file = nullptr;
  std::vector<std::string> errors;
  bool in_for_iter = false;
  std::shared_ptr<Type> for_it_type;
  std::shared_ptr<Type> for_it_index_type;
  std::vector<std::unique_ptr<StructType>> structs_;
  std::vector<std::unique_ptr<EnumType>> enums_;
  std::vector<std::unique_ptr<ProcType>> procs_;

  void add_error(SourceLoc loc, const std::string& msg);
  std::shared_ptr<Type> resolve_type_expr(const TypeExpr& te);
  Symbol* resolve_ident(const std::string& name);
  std::optional<int64_t> try_eval_int_const(const Expr& e);
  bool check_declaration(Decl& d);
  bool check_declaration(Decl& d, bool register_proc_only);  // true = pass 1: register proc only
  bool check_statement(Stmt& s);
  bool check_expression(Expr& e, std::shared_ptr<Type>* out_type = nullptr);
  bool check_file(File& f);
};

}  // namespace jai
