#pragma once

#include "ast.hpp"
#include "sema.hpp"
#include "types.hpp"
#include <memory>
#include <unordered_map>
#include <vector>

namespace jai {

struct RunValue {
  enum class Tag { None, I64, F64, Str, ArrayF64, ArrayI64 };
  Tag tag = Tag::None;
  int64_t i64 = 0;
  double f64 = 0;
  std::string str_val;
  std::vector<double> array_f64;
  std::vector<int64_t> array_i64;
};

class CompileTimeEval {
 public:
  CompileTimeEval(File* file, SemaContext* sema);
  bool eval_expr(Expr& e, RunValue& out);
  bool eval_stmt(Stmt& s);
  bool eval_proc(ProcDecl& p, RunValue& result);
  void set_local(const std::string& name, RunValue v);
  RunValue get_local(const std::string& name) const;

 private:
  File* file_ = nullptr;
  SemaContext* sema_ = nullptr;
  std::unordered_map<std::string, RunValue> locals_;
  std::unordered_map<std::string, ProcDecl*> procs_;
  RunValue return_value_;
  bool has_return_ = false;
};

RunValue evaluate_compile_time_function(const std::string& name, File* file, SemaContext* sema);

}  // namespace jai
