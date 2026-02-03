#pragma once

#include "ast.hpp"
#include "interpreter.hpp"
#include "sema.hpp"
#include "types.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <memory>
#include <set>
#include <string>
#include <unordered_map>

namespace llvm {
class Value;
class Type;
class Function;
}  // namespace llvm

namespace jai {

class CodeGen {
 public:
  CodeGen(const std::string& module_name);
  bool run(File& file, SemaContext& sema);
  bool write_ir_to_file(const std::string& path);
  bool write_obj_to_file(const std::string& path);
  llvm::Module* module() { return module_.get(); }

 private:
  llvm::Type* llvm_type(std::shared_ptr<Type> t);
  llvm::Value* emit_expr(Expr& e);
  llvm::Value* emit_lvalue_addr(Expr& e);
  void emit_stmt(Stmt& s);
  void emit_decl(Decl& d);
  llvm::Function* get_or_declare_main();
  llvm::Function* get_or_declare_proc(ProcDecl& p);
  llvm::Function* get_or_declare_extern_proc(ProcDecl& p);
  void declare_print();
  void emit_type_table();

  std::unique_ptr<llvm::LLVMContext> context_;
  std::unique_ptr<llvm::Module> module_;
  std::unique_ptr<llvm::IRBuilder<>> builder_;
  std::unordered_map<std::string, llvm::Value*> locals_;
  std::unordered_map<std::string, llvm::Value*> globals_;
  std::unordered_map<std::string, llvm::Function*> functions_;
  llvm::Function* current_function_ = nullptr;
  File* file_ = nullptr;
  SemaContext* sema_ = nullptr;
  std::vector<std::string> errors_;
  llvm::GlobalVariable* type_table_global_ = nullptr;
  llvm::StructType* type_info_struct_ty_ = nullptr;
  llvm::StructType* type_table_struct_ty_ = nullptr;
  llvm::StructType* any_struct_ty_ = nullptr;
  std::set<std::string> force_inline_procs_;
  std::set<std::string> force_no_inline_procs_;
  std::unordered_map<std::string, size_t> global_array_counts_;
};

}  // namespace jai
