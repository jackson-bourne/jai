#include "codegen.hpp"
#include <llvm/Analysis/AssumptionCache.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/Utils/PromoteMemToReg.h>

namespace jai {

static llvm::Type* i8ptr(llvm::LLVMContext& ctx) {
  return llvm::PointerType::get(ctx, 0);
}

CodeGen::CodeGen(const std::string& module_name)
    : context_(std::make_unique<llvm::LLVMContext>()),
      module_(std::make_unique<llvm::Module>(module_name, *context_)),
      builder_(std::make_unique<llvm::IRBuilder<>>(*context_)) {}

llvm::Type* CodeGen::llvm_type(std::shared_ptr<Type> t) {
  if (!t) return llvm::Type::getInt64Ty(*context_);
  switch (t->kind) {
    case TypeKind::Int:
    case TypeKind::Int64:
      return llvm::Type::getInt64Ty(*context_);
    case TypeKind::Int8:
      return llvm::Type::getInt8Ty(*context_);
    case TypeKind::Int16:
      return llvm::Type::getInt16Ty(*context_);
    case TypeKind::Int32:
      return llvm::Type::getInt32Ty(*context_);
    case TypeKind::UInt8:
      return llvm::Type::getInt8Ty(*context_);
    case TypeKind::UInt16:
      return llvm::Type::getInt16Ty(*context_);
    case TypeKind::UInt32:
      return llvm::Type::getInt32Ty(*context_);
    case TypeKind::UInt64:
      return llvm::Type::getInt64Ty(*context_);
    case TypeKind::Float:
      return llvm::Type::getFloatTy(*context_);
    case TypeKind::Double:
      return llvm::Type::getDoubleTy(*context_);
    case TypeKind::Bool:
      return llvm::Type::getInt1Ty(*context_);
    case TypeKind::String:
      return i8ptr(*context_);
    case TypeKind::Pointer:
    case TypeKind::OwnedPointer:
      if (t->element_type)
        return llvm::PointerType::get(*context_, 0);
      return i8ptr(*context_);
    case TypeKind::ArrayFixed:
      if (t->element_type && t->fixed_count > 0)
        return llvm::ArrayType::get(llvm_type(t->element_type), t->fixed_count);
      return i8ptr(*context_);
    case TypeKind::ArrayDynamic:
      return i8ptr(*context_);
    case TypeKind::Struct:
    case TypeKind::StructSOA:
      return i8ptr(*context_);
    case TypeKind::Enum:
      return llvm::Type::getInt64Ty(*context_);
    case TypeKind::Procedure:
      return llvm::Type::getInt64Ty(*context_);
    case TypeKind::Any:
      return any_struct_ty_ ? static_cast<llvm::Type*>(any_struct_ty_)
                            : llvm::Type::getInt64Ty(*context_);
    case TypeKind::TypeInfo:
      return type_info_struct_ty_ ? static_cast<llvm::Type*>(type_info_struct_ty_)
                                   : llvm::Type::getInt64Ty(*context_);
    case TypeKind::TypeInfoTag:
      return llvm::Type::getInt32Ty(*context_);
    case TypeKind::TypeTableView:
      return type_table_struct_ty_ ? static_cast<llvm::Type*>(type_table_struct_ty_)
                                   : llvm::Type::getInt64Ty(*context_);
    default:
      return llvm::Type::getInt64Ty(*context_);
  }
}

void CodeGen::declare_print() {
  if (module_->getFunction("printf")) return;
  std::vector<llvm::Type*> args;
  args.push_back(i8ptr(*context_));
  llvm::FunctionType* ft =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*context_), args, true);
  llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "printf", module_.get());
}

llvm::Function* CodeGen::get_or_declare_main() {
  if (auto* f = module_->getFunction("main")) return f;
  llvm::FunctionType* ft =
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*context_), false);
  return llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "main", module_.get());
}

void CodeGen::emit_type_table() {
  type_info_struct_ty_ = llvm::StructType::create(
      *context_, {llvm::Type::getInt32Ty(*context_), i8ptr(*context_)}, "Type_Info");
  type_table_struct_ty_ = llvm::StructType::create(
      *context_, {i8ptr(*context_), llvm::Type::getInt64Ty(*context_)}, "Type_Table");

  enum { TAG_INT = 0, TAG_FLOAT = 1, TAG_BOOL = 2, TAG_STRING = 3, TAG_STRUCT = 4, TAG_ENUM = 5, TAG_PROCEDURE = 6 };
  std::vector<llvm::Constant*> entries;
  auto add = [&](int tag, const std::string& name) {
    llvm::GlobalVariable* gs = builder_->CreateGlobalString(name, "", 0, module_.get());
    llvm::Constant* name_const = llvm::ConstantExpr::getPointerCast(gs, i8ptr(*context_));
    entries.push_back(llvm::ConstantStruct::get(
        type_info_struct_ty_,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), tag),
        name_const));
  };
  add(TAG_INT, "int");
  add(TAG_FLOAT, "float");
  add(TAG_BOOL, "bool");
  add(TAG_STRING, "string");
  if (sema_) {
    for (const auto& st : sema_->structs_)
      if (st) add(TAG_STRUCT, st->name);
    for (const auto& en : sema_->enums_)
      if (en) add(TAG_ENUM, en->name);
    for (const auto& pr : sema_->procs_)
      if (pr) add(TAG_PROCEDURE, pr->name);
  }

  if (entries.empty()) {
    llvm::GlobalVariable* gs = builder_->CreateGlobalString("int", "", 0, module_.get());
    llvm::Constant* name_const = llvm::ConstantExpr::getPointerCast(gs, i8ptr(*context_));
    entries.push_back(llvm::ConstantStruct::get(
        type_info_struct_ty_,
        llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0),
        name_const));
  }

  llvm::ArrayType* arr_ty = llvm::ArrayType::get(type_info_struct_ty_, entries.size());
  llvm::Constant* arr_init = llvm::ConstantArray::get(arr_ty, entries);
  llvm::GlobalVariable* arr_gv = new llvm::GlobalVariable(
      *module_, arr_ty, true, llvm::GlobalValue::InternalLinkage, arr_init, "type_table_data");
  std::vector<llvm::Constant*> table_vals;
  table_vals.push_back(llvm::ConstantExpr::getPointerCast(arr_gv, i8ptr(*context_)));
  table_vals.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), entries.size()));
  llvm::Constant* table_init = llvm::ConstantStruct::get(type_table_struct_ty_, table_vals);
  type_table_global_ = new llvm::GlobalVariable(
      *module_, type_table_struct_ty_, true, llvm::GlobalValue::InternalLinkage, table_init, "_type_table");
}

llvm::Value* CodeGen::emit_expr(Expr& e) {
  switch (e.kind) {
    case Expr::Kind::IntLiteral:
      return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), e.int_val, true);
    case Expr::Kind::FloatLiteral:
      return llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), e.float_val);
    case Expr::Kind::StringLiteral: {
      std::string s = e.string_val;
      if (s.size() >= 2 && s.front() == '"' && s.back() == '"')
        s = s.substr(1, s.size() - 2);
      std::string out;
      for (size_t i = 0; i < s.size(); ++i) {
        if (s[i] == '\\' && i + 1 < s.size()) {
          switch (s[i + 1]) {
            case 'n': out += '\n'; ++i; break;
            case 't': out += '\t'; ++i; break;
            case 'r': out += '\r'; ++i; break;
            case '0': /* \0 would truncate; leave as-is */ out += '\\'; out += '0'; ++i; break;
            case '\\': out += '\\'; ++i; break;
            case '"': out += '"'; ++i; break;
            default: out += s[i]; break;
          }
        } else
          out += s[i];
      }
      llvm::GlobalVariable* gv = builder_->CreateGlobalString(out, "", 0, module_.get());
      return builder_->CreateConstInBoundsGEP2_32(gv->getValueType(), gv, 0, 0);
    }
    case Expr::Kind::BoolLiteral:
      return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*context_), e.bool_val ? 1 : 0);
    case Expr::Kind::NullLiteral:
      return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(i8ptr(*context_)));
    case Expr::Kind::Ident: {
      if (e.ident == "print") return nullptr;
      if (sema_) {
        Symbol* sym = sema_->resolve_ident(e.ident);
        if (sym && sym->kind == Symbol::Kind::Const && sym->const_int_value)
          return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), *sym->const_int_value);
      }
      auto lit = locals_.find(e.ident);
      if (lit != locals_.end()) {
        llvm::Type* t = llvm::Type::getInt64Ty(*context_);
        if (sema_) {
          Symbol* sym = sema_->resolve_ident(e.ident);
          if (sym && sym->type) t = llvm_type(sym->type);
        }
        return builder_->CreateLoad(t, lit->second);
      }
      auto git = globals_.find(e.ident);
      if (git != globals_.end()) {
        llvm::Value* gv = git->second;
        if (auto* gvar = llvm::dyn_cast<llvm::GlobalVariable>(gv)) {
          if (gvar->getValueType()->isArrayTy())
            return gv;
          return builder_->CreateLoad(gvar->getValueType(), gv);
        }
        return gv;
      }
      if (e.ident == "_type_table" && type_table_global_)
        return type_table_global_;
      return nullptr;
    }
    case Expr::Kind::Binary: {
      if (!e.lhs || !e.rhs) return nullptr;
      if (e.op == TokenKind::Eq) {
        llvm::Value* ptr = emit_lvalue_addr(*e.lhs);
        if (!ptr) return nullptr;
        llvm::Value* R = emit_expr(*e.rhs);
        if (!R) return nullptr;
        builder_->CreateStore(R, ptr);
        return R;
      }
      llvm::Value* L = emit_expr(*e.lhs);
      llvm::Value* R = emit_expr(*e.rhs);
      if (!L || !R) return nullptr;
      switch (e.op) {
        case TokenKind::Plus:
          return builder_->CreateAdd(L, R);
        case TokenKind::Minus:
          return builder_->CreateSub(L, R);
        case TokenKind::Star:
          return builder_->CreateMul(L, R);
        case TokenKind::Slash:
          return builder_->CreateSDiv(L, R);
        case TokenKind::EqEq:
          return builder_->CreateICmpEQ(L, R);
        case TokenKind::NotEq:
          return builder_->CreateICmpNE(L, R);
        case TokenKind::Less:
          return builder_->CreateICmpSLT(L, R);
        case TokenKind::LessEq:
          return builder_->CreateICmpSLE(L, R);
        case TokenKind::Greater:
          return builder_->CreateICmpSGT(L, R);
        case TokenKind::GreaterEq:
          return builder_->CreateICmpSGE(L, R);
        default:
          return builder_->CreateAdd(L, R);
      }
    }
    case Expr::Kind::Call: {
      if (!e.lhs) return nullptr;
      if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "print") {
        declare_print();
        llvm::Function* printf_fn = module_->getFunction("printf");
        if (!printf_fn) return nullptr;
        std::vector<llvm::Value*> args;
        for (size_t i = 0; i < e.args.size(); ++i) {
          if (!e.args[i]) continue;
          llvm::Value* v = emit_expr(*e.args[i]);
          if (v) args.push_back(v);
        }
        if (args.empty()) return nullptr;
        return builder_->CreateCall(printf_fn, args);
      }
      llvm::Value* callee = emit_expr(*e.lhs);
      if (!callee) return nullptr;
      std::vector<llvm::Value*> args;
      for (auto& a : e.args)
        if (a) {
          llvm::Value* v = emit_expr(*a);
          if (v) args.push_back(v);
        }
      return builder_->CreateCall(llvm::dyn_cast<llvm::Function>(callee), args);
    }
    case Expr::Kind::Index: {
      if (!e.lhs || !e.index_expr) return nullptr;
      if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "_type_table" &&
          type_table_global_ && type_info_struct_ty_) {
        llvm::Value* data_gep = builder_->CreateStructGEP(type_table_struct_ty_, type_table_global_, 0);
        llvm::Value* data = builder_->CreateLoad(i8ptr(*context_), data_gep);
        llvm::Value* idx = emit_expr(*e.index_expr);
        if (!idx) return nullptr;
        llvm::Value* elem_ptr = builder_->CreateGEP(type_info_struct_ty_, data, idx);
        return elem_ptr;
      }
      llvm::Value* base = emit_expr(*e.lhs);
      llvm::Value* idx = emit_expr(*e.index_expr);
      if (!base || !idx) return nullptr;
      llvm::Type* elem_ty = nullptr;
      if (auto* gv = llvm::dyn_cast<llvm::GlobalVariable>(base))
        elem_ty = gv->getValueType();
      else if (auto* ai = llvm::dyn_cast<llvm::AllocaInst>(base))
        elem_ty = ai->getAllocatedType();
      if (elem_ty && llvm::isa<llvm::ArrayType>(elem_ty)) {
        auto* at = llvm::cast<llvm::ArrayType>(elem_ty);
        std::vector<llvm::Value*> gep_idx;
        gep_idx.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0));
        gep_idx.push_back(idx);
        llvm::Value* elem_ptr = builder_->CreateGEP(at, base, gep_idx);
        return builder_->CreateLoad(at->getElementType(), elem_ptr);
      }
      if (elem_ty) {
        std::vector<llvm::Value*> gep_idx;
        gep_idx.push_back(idx);
        llvm::Value* elem_ptr = builder_->CreateGEP(elem_ty, base, gep_idx);
        return builder_->CreateLoad(elem_ty, elem_ptr);
      }
      return nullptr;
    }
    case Expr::Kind::Member: {
      if (!e.lhs) return nullptr;
      if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "_type_table" &&
          type_table_global_ && type_table_struct_ty_) {
        if (e.member_name == "count") {
          llvm::Value* gep = builder_->CreateStructGEP(type_table_struct_ty_, type_table_global_, 1);
          return builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), gep);
        }
        if (e.member_name == "data") {
          llvm::Value* gep = builder_->CreateStructGEP(type_table_struct_ty_, type_table_global_, 0);
          return builder_->CreateLoad(i8ptr(*context_), gep);
        }
      }
      if (e.lhs->kind == Expr::Kind::Ident && e.member_name == "count") {
        auto it = global_array_counts_.find(e.lhs->ident);
        if (it != global_array_counts_.end())
          return llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), static_cast<uint64_t>(it->second));
      }
      llvm::Value* base = nullptr;
      if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "it") {
        auto lit = locals_.find("it");
        if (lit != locals_.end()) base = lit->second;
      }
      if (!base) base = emit_expr(*e.lhs);
      if (!base) return nullptr;
      if (type_info_struct_ty_ && (e.member_name == "name" || e.member_name == "type")) {
        bool is_it = (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "it");
        bool is_table_index = (e.lhs->kind == Expr::Kind::Index && e.lhs->lhs &&
                              e.lhs->lhs->kind == Expr::Kind::Ident && e.lhs->lhs->ident == "_type_table");
        if (is_it || is_table_index) {
          int idx = (e.member_name == "type") ? 0 : 1;
          llvm::Value* gep = builder_->CreateStructGEP(type_info_struct_ty_, base, idx);
          llvm::Type* field_ty = type_info_struct_ty_->getElementType(idx);
          return builder_->CreateLoad(field_ty, gep);
        }
      }
      if (e.member_name == "count") {
        return nullptr;
      }
      return nullptr;
    }
    case Expr::Kind::Cast: {
      if (!e.operand) return nullptr;
      llvm::Value* v = emit_expr(*e.operand);
      if (!v) return nullptr;
      return v;
    }
    case Expr::Kind::Run:
    case Expr::Kind::InitList:
    default:
      return nullptr;
  }
}

llvm::Value* CodeGen::emit_lvalue_addr(Expr& e) {
  switch (e.kind) {
    case Expr::Kind::Ident: {
      if (e.ident == "print") return nullptr;
      auto lit = locals_.find(e.ident);
      if (lit != locals_.end()) return lit->second;
      auto git = globals_.find(e.ident);
      if (git != globals_.end()) return git->second;
      return nullptr;
    }
    case Expr::Kind::Index: {
      if (!e.lhs || !e.index_expr) return nullptr;
      llvm::Value* base = emit_expr(*e.lhs);
      llvm::Value* idx = emit_expr(*e.index_expr);
      if (!base || !idx) return nullptr;
      llvm::Type* elem_ty = nullptr;
      if (auto* gv = llvm::dyn_cast<llvm::GlobalVariable>(base))
        elem_ty = gv->getValueType();
      else if (auto* ai = llvm::dyn_cast<llvm::AllocaInst>(base))
        elem_ty = ai->getAllocatedType();
      if (elem_ty && llvm::isa<llvm::ArrayType>(elem_ty)) {
        auto* at = llvm::cast<llvm::ArrayType>(elem_ty);
        std::vector<llvm::Value*> gep_idx;
        gep_idx.push_back(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0));
        gep_idx.push_back(idx);
        return builder_->CreateGEP(at, base, gep_idx);
      }
      if (elem_ty) {
        std::vector<llvm::Value*> gep_idx;
        gep_idx.push_back(idx);
        return builder_->CreateGEP(elem_ty, base, gep_idx);
      }
      return nullptr;
    }
    default:
      return nullptr;
  }
}

void CodeGen::emit_stmt(Stmt& s) {
  switch (s.kind) {
    case Stmt::Kind::Block: {
      for (auto& st : s.block_body)
        if (st) emit_stmt(*st);
      break;
    }
    case Stmt::Kind::Expr:
      if (s.expr) emit_expr(*s.expr);
      break;
    case Stmt::Kind::VarDecl: {
      llvm::Type* t = llvm::Type::getInt64Ty(*context_);
      if (s.var_type && sema_) {
        auto jt = sema_->resolve_type_expr(*s.var_type);
        if (jt) t = llvm_type(jt);
      }
      llvm::Value* alloca;
      if (current_function_) {
        llvm::BasicBlock* entry = &current_function_->getEntryBlock();
        llvm::IRBuilderBase::InsertPointGuard guard(*builder_);
        builder_->SetInsertPoint(entry, entry->getFirstInsertionPt());
        alloca = builder_->CreateAlloca(t, nullptr, s.var_name);
      } else {
        alloca = builder_->CreateAlloca(t, nullptr, s.var_name);
      }
      if (s.var_init) {
        llvm::Value* v = emit_expr(*s.var_init);
        if (v) builder_->CreateStore(v, alloca);
      }
      locals_[s.var_name] = alloca;
      break;
    }
    case Stmt::Kind::If: {
      if (!s.if_cond || !s.if_then) break;
      llvm::Value* cond = emit_expr(*s.if_cond);
      if (!cond) break;
      cond = builder_->CreateICmpNE(cond, llvm::ConstantInt::get(cond->getType(), 0));
      llvm::Function* f = builder_->GetInsertBlock()->getParent();
      llvm::BasicBlock* then_bb = llvm::BasicBlock::Create(*context_, "then", f);
      llvm::BasicBlock* else_bb = s.if_else ? llvm::BasicBlock::Create(*context_, "else", f) : nullptr;
      llvm::BasicBlock* merge_bb = llvm::BasicBlock::Create(*context_, "ifcont", f);
      if (else_bb)
        builder_->CreateCondBr(cond, then_bb, else_bb);
      else
        builder_->CreateCondBr(cond, then_bb, merge_bb);
      builder_->SetInsertPoint(then_bb);
      emit_stmt(*s.if_then);
      builder_->CreateBr(merge_bb);
      if (else_bb) {
        builder_->SetInsertPoint(else_bb);
        emit_stmt(*s.if_else);
        builder_->CreateBr(merge_bb);
      }
      builder_->SetInsertPoint(merge_bb);
      break;
    }
    case Stmt::Kind::ForRange: {
      if (!s.for_range_start || !s.for_range_end || !s.for_body) break;
      llvm::Value* start = emit_expr(*s.for_range_start);
      llvm::Value* end_incl = emit_expr(*s.for_range_end);
      if (!start || !end_incl) break;
      llvm::Value* end = builder_->CreateAdd(end_incl, llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1));
      llvm::Function* f = builder_->GetInsertBlock()->getParent();
      llvm::Value* i_alloca = builder_->CreateAlloca(llvm::Type::getInt64Ty(*context_), nullptr, s.for_var_name);
      builder_->CreateStore(start, i_alloca);
      llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "for.cond", f);
      llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "for.body", f);
      llvm::BasicBlock* after_bb = llvm::BasicBlock::Create(*context_, "for.end", f);
      builder_->CreateBr(cond_bb);
      builder_->SetInsertPoint(cond_bb);
      llvm::Value* cur = builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), i_alloca);
      llvm::Value* cond = builder_->CreateICmpSLT(cur, end);
      builder_->CreateCondBr(cond, body_bb, after_bb);
      builder_->SetInsertPoint(body_bb);
      locals_[s.for_var_name] = i_alloca;
      emit_stmt(*s.for_body);
      llvm::Value* next = builder_->CreateAdd(builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), i_alloca), llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1));
      builder_->CreateStore(next, i_alloca);
      builder_->CreateBr(cond_bb);
      builder_->SetInsertPoint(after_bb);
      locals_.erase(s.for_var_name);
      break;
    }
    case Stmt::Kind::ForIter: {
      if (!s.for_iter_expr || !s.for_body) break;
      bool is_type_table = (s.for_iter_expr->kind == Expr::Kind::Ident &&
                            s.for_iter_expr->ident == "_type_table");
      if (is_type_table && type_table_global_ && type_table_struct_ty_ && type_info_struct_ty_) {
        llvm::Value* count_gep = builder_->CreateStructGEP(type_table_struct_ty_, type_table_global_, 1);
        llvm::Value* count = builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), count_gep);
        llvm::Function* f = builder_->GetInsertBlock()->getParent();
        llvm::Value* i_alloca = builder_->CreateAlloca(llvm::Type::getInt64Ty(*context_), nullptr, "for.i");
        builder_->CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 0), i_alloca);
        llvm::BasicBlock* cond_bb = llvm::BasicBlock::Create(*context_, "for.cond", f);
        llvm::BasicBlock* body_bb = llvm::BasicBlock::Create(*context_, "for.body", f);
        llvm::BasicBlock* after_bb = llvm::BasicBlock::Create(*context_, "for.end", f);
        builder_->CreateBr(cond_bb);
        builder_->SetInsertPoint(cond_bb);
        llvm::Value* cur = builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), i_alloca);
        llvm::Value* cond = builder_->CreateICmpSLT(cur, count);
        builder_->CreateCondBr(cond, body_bb, after_bb);
        builder_->SetInsertPoint(body_bb);
        llvm::Value* data_gep = builder_->CreateStructGEP(type_table_struct_ty_, type_table_global_, 0);
        llvm::Value* data = builder_->CreateLoad(i8ptr(*context_), data_gep);
        llvm::Value* it_ptr = builder_->CreateGEP(type_info_struct_ty_, data, cur);
        llvm::Value* it_index_val = builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), i_alloca);
        llvm::Value* it_alloca = builder_->CreateAlloca(type_info_struct_ty_, nullptr, "it");
        builder_->CreateStore(builder_->CreateLoad(type_info_struct_ty_, it_ptr), it_alloca);
        llvm::Value* it_index_alloca = builder_->CreateAlloca(llvm::Type::getInt64Ty(*context_), nullptr, "it_index");
        builder_->CreateStore(it_index_val, it_index_alloca);
        locals_["it"] = it_alloca;
        locals_["it_index"] = it_index_alloca;
        emit_stmt(*s.for_body);
        llvm::Value* next = builder_->CreateAdd(
            builder_->CreateLoad(llvm::Type::getInt64Ty(*context_), i_alloca),
            llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), 1));
        builder_->CreateStore(next, i_alloca);
        builder_->CreateBr(cond_bb);
        builder_->SetInsertPoint(after_bb);
        locals_.erase("it");
        locals_.erase("it_index");
      }
      break;
    }
    case Stmt::Kind::While:
      break;
    case Stmt::Kind::Return: {
      llvm::Value* v = nullptr;
      if (s.return_expr) v = emit_expr(*s.return_expr);
      if (v)
        builder_->CreateRet(v);
      else
        builder_->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0));
      break;
    }
    case Stmt::Kind::Defer:
      if (s.defer_body) emit_stmt(*s.defer_body);
      break;
    default:
      break;
  }
}

void CodeGen::emit_decl(Decl& d) {
  switch (d.kind) {
    case Decl::Kind::Var: {
      if (d.var_init && d.var_init->kind == Expr::Kind::Run &&
          d.var_init->run_expr && d.var_init->run_expr->kind == Expr::Kind::Call &&
          d.var_init->run_expr->lhs && d.var_init->run_expr->lhs->kind == Expr::Kind::Ident) {
        std::string fn_name = d.var_init->run_expr->lhs->ident;
        RunValue result = evaluate_compile_time_function(fn_name, file_, sema_);
        if (result.tag == RunValue::Tag::ArrayF64 && !result.array_f64.empty()) {
          size_t n = result.array_f64.size();
          std::vector<llvm::Constant*> elts;
          for (double v : result.array_f64)
            elts.push_back(llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), v));
          llvm::ArrayType* at = llvm::ArrayType::get(llvm::Type::getDoubleTy(*context_), n);
          llvm::Constant* init = llvm::ConstantArray::get(at, elts);
          llvm::GlobalVariable* gv = new llvm::GlobalVariable(
              *module_, at, true, llvm::GlobalValue::InternalLinkage, init, d.var_name);
          globals_[d.var_name] = gv;
          global_array_counts_[d.var_name] = n;
        }
      } else if (!d.var_name.empty()) {
        llvm::Type* t = llvm::Type::getInt64Ty(*context_);
        if (d.var_type && sema_) {
          auto jt = sema_->resolve_type_expr(*d.var_type);
          if (jt) t = llvm_type(jt);
        }
        llvm::Constant* init = nullptr;
        if (d.var_init) {
          if (d.var_init->kind == Expr::Kind::IntLiteral)
            init = llvm::ConstantInt::get(llvm::Type::getInt64Ty(*context_), static_cast<uint64_t>(d.var_init->int_val));
          else if (d.var_init->kind == Expr::Kind::FloatLiteral)
            init = llvm::ConstantFP::get(llvm::Type::getDoubleTy(*context_), d.var_init->float_val);
        }
        if (!init)
          init = (t->isIntegerTy()) ? llvm::ConstantInt::get(t, 0) : llvm::ConstantFP::get(t, 0.0);
        if (init->getType() != t) {
          if (t->isIntegerTy()) init = llvm::ConstantInt::get(t, 0);
          else init = llvm::ConstantFP::get(t, 0.0);
        }
        llvm::GlobalVariable* gv = new llvm::GlobalVariable(
            *module_, t, false, llvm::GlobalValue::InternalLinkage, init, d.var_name);
        globals_[d.var_name] = gv;
      }
      break;
    }
    case Decl::Kind::DirectiveInline:
      if (!d.directive_inline_proc.empty())
        force_inline_procs_.insert(d.directive_inline_proc);
      break;
    case Decl::Kind::DirectiveNoInline:
      if (!d.directive_inline_proc.empty())
        force_no_inline_procs_.insert(d.directive_inline_proc);
      break;
    case Decl::Kind::Proc: {
      if (!d.proc) return;
      ProcDecl& p = *d.proc;
      if (p.name != "main") return;
      llvm::Function* main_fn = get_or_declare_main();
      // When multiple files define main (e.g. #load "loaded.jai" then main in this file),
      // last main wins: replace any existing body.
      if (!main_fn->empty()) {
        std::vector<llvm::BasicBlock*> blocks;
        for (llvm::BasicBlock& B : *main_fn)
          blocks.push_back(&B);
        for (llvm::BasicBlock* B : blocks)
          B->eraseFromParent();
      }
      bool want_inline = p.inline_requested || force_inline_procs_.count(p.name);
      bool want_no_inline = p.no_inline_requested || force_no_inline_procs_.count(p.name);
      if (want_inline)
        main_fn->addFnAttr(llvm::Attribute::AlwaysInline);
      if (want_no_inline)
        main_fn->addFnAttr(llvm::Attribute::NoInline);
      llvm::BasicBlock* bb = llvm::BasicBlock::Create(*context_, "entry", main_fn);
      builder_->SetInsertPoint(bb);
      current_function_ = main_fn;
      locals_.clear();
      if (p.body) emit_stmt(*p.body);
      if (!builder_->GetInsertBlock()->getTerminator())
        builder_->CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context_), 0));
      current_function_ = nullptr;
      break;
    }
    default:
      break;
  }
}

bool CodeGen::run(File& file, SemaContext& sema) {
  file_ = &file;
  sema_ = &sema;
  declare_print();
  emit_type_table();
  for (auto& d : file.declarations)
    if (d) emit_decl(*d);

  // Promote allocas to SSA registers (mem2reg) so variables assigned in
  // different branches have correct values at merge points (e.g. z in if/else).
  for (llvm::Function& F : *module_) {
    if (F.isDeclaration() || F.empty()) continue;
    llvm::DominatorTree DT(F);
    llvm::AssumptionCache AC(F);
    std::vector<llvm::AllocaInst*> allocas;
    for (llvm::Instruction& I : F.getEntryBlock()) {
      if (auto* AI = llvm::dyn_cast<llvm::AllocaInst>(&I))
        if (llvm::isAllocaPromotable(AI))
          allocas.push_back(AI);
    }
    if (!allocas.empty())
      llvm::PromoteMemToReg(allocas, DT, &AC);
  }

  return !llvm::verifyModule(*module_, &llvm::errs());
}

bool CodeGen::write_ir_to_file(const std::string& path) {
  std::error_code EC;
  llvm::raw_fd_ostream dest(path, EC, llvm::sys::fs::OF_None);
  if (EC) return false;
  module_->print(dest, nullptr);
  return true;
}

bool CodeGen::write_obj_to_file(const std::string& path) {
  (void)path;
  return false;
}

}  // namespace jai
