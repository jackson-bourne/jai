#pragma once

#include "ast.hpp"
#include "sema.hpp"
#include <cstdint>
#include <vector>

namespace jai {

enum class BcOp : uint8_t {
  Nop,
  PushI64,
  PushF64,
  PushLocal,
  PopLocal,
  Add,
  Sub,
  Mul,
  Div,
  Call,
  Return,
  GetArrayElem,
  SetArrayElem,
  NewArray,
  ForRangeInit,
  ForRangeNext,
  Branch,
  BranchIf,
};

struct BcInstr {
  BcOp op = BcOp::Nop;
  int32_t imm = 0;
};

struct BcProgram {
  std::vector<BcInstr> code;
  std::vector<int64_t> constants_i;
  std::vector<double> constants_f;
};

bool compile_expr_to_bytecode(Expr& e, BcProgram& out, SemaContext* sema);
bool compile_stmt_to_bytecode(Stmt& s, BcProgram& out, SemaContext* sema);
bool compile_proc_to_bytecode(ProcDecl& p, BcProgram& out, SemaContext* sema);

}  // namespace jai
