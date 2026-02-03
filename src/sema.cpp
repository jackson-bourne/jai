#include "sema.hpp"
#include <sstream>

namespace jai {

void Scope::add(const std::string& name, Symbol sym) {
  symbols_[name] = std::move(sym);
}

Symbol* Scope::find(const std::string& name) {
  auto it = symbols_.find(name);
  if (it != symbols_.end()) return &it->second;
  if (parent_) return parent_->find(name);
  return nullptr;
}

const Symbol* Scope::find(const std::string& name) const {
  auto it = symbols_.find(name);
  if (it != symbols_.end()) return &it->second;
  if (parent_) return parent_->find(name);
  return nullptr;
}

void SemaContext::add_error(SourceLoc loc, const std::string& msg) {
  std::ostringstream os;
  if (file && !file->path.empty()) os << file->path << ":";
  os << loc.line << ":" << loc.column << ": " << msg;
  errors.push_back(os.str());
}

std::shared_ptr<Type> SemaContext::resolve_type_expr(const TypeExpr& te) {
  switch (te.kind) {
    case TypeExpr::Kind::Ident: {
      const std::string& n = te.name;
      if (n == "int") return type_int();
      if (n == "float") return type_float();
      if (n == "bool") return type_bool();
      if (n == "string") return type_string();
      if (n == "Any") return type_any();
      Symbol* s = resolve_ident(n);
      if (s) {
        if (s->kind == Symbol::Kind::Struct && s->struct_type) {
          auto t = std::make_shared<Type>();
          t->kind = s->struct_type->soa ? TypeKind::StructSOA : TypeKind::Struct;
          t->name = s->struct_type->name;
          t->struct_type = s->struct_type;
          return t;
        }
        if (s->kind == Symbol::Kind::Enum && s->enum_type) {
          auto t = std::make_shared<Type>();
          t->kind = TypeKind::Enum;
          t->name = s->enum_type->name;
          t->enum_type = s->enum_type;
          return t;
        }
      }
      return nullptr;
    }
    case TypeExpr::Kind::Pointer: {
      if (!te.element_type) return nullptr;
      auto et = resolve_type_expr(*te.element_type);
      if (!et) return nullptr;
      auto t = std::make_shared<Type>();
      t->kind = TypeKind::Pointer;
      t->element_type = et;
      return t;
    }
    case TypeExpr::Kind::OwnedPointer: {
      if (!te.element_type) return nullptr;
      auto et = resolve_type_expr(*te.element_type);
      if (!et) return nullptr;
      auto t = std::make_shared<Type>();
      t->kind = TypeKind::OwnedPointer;
      t->element_type = et;
      return t;
    }
    case TypeExpr::Kind::ArrayFixed: {
      if (!te.element_type) return nullptr;
      auto et = resolve_type_expr(*te.element_type);
      if (!et) return nullptr;
      int count = te.fixed_count;
      if (count <= 0 && te.count_expr) {
        auto opt = try_eval_int_const(*te.count_expr);
        if (opt) count = static_cast<int>(*opt);
      }
      auto t = std::make_shared<Type>();
      t->kind = TypeKind::ArrayFixed;
      t->element_type = et;
      t->fixed_count = count > 0 ? count : 0;
      return t;
    }
    case TypeExpr::Kind::ArrayDynamic: {
      if (!te.element_type) return nullptr;
      auto et = resolve_type_expr(*te.element_type);
      if (!et) return nullptr;
      auto t = std::make_shared<Type>();
      t->kind = TypeKind::ArrayDynamic;
      t->element_type = et;
      return t;
    }
    case TypeExpr::Kind::Optional:
      return nullptr;
  }
  return nullptr;
}

Symbol* SemaContext::resolve_ident(const std::string& name) {
  if (scope) return scope->find(name);
  return nullptr;
}

std::optional<int64_t> SemaContext::try_eval_int_const(const Expr& e) {
  switch (e.kind) {
    case Expr::Kind::IntLiteral:
      return e.int_val;
    case Expr::Kind::Ident: {
      Symbol* s = resolve_ident(e.ident);
      if (s && s->kind == Symbol::Kind::Const && s->const_int_value)
        return *s->const_int_value;
      return std::nullopt;
    }
    case Expr::Kind::Binary: {
      if (!e.lhs || !e.rhs) return std::nullopt;
      auto l = try_eval_int_const(*e.lhs);
      auto r = try_eval_int_const(*e.rhs);
      if (!l || !r) return std::nullopt;
      switch (e.op) {
        case TokenKind::Plus: return *l + *r;
        case TokenKind::Minus: return *l - *r;
        case TokenKind::Star: return *l * *r;
        case TokenKind::Slash:
          return *r != 0 ? std::optional<int64_t>(*l / *r) : std::nullopt;
        case TokenKind::Percent:
          return *r != 0 ? std::optional<int64_t>(*l % *r) : std::nullopt;
        default: return std::nullopt;
      }
    }
    default:
      return std::nullopt;
  }
}

bool SemaContext::check_expression(Expr& e, std::shared_ptr<Type>* out_type) {
  switch (e.kind) {
    case Expr::Kind::IntLiteral: {
      if (out_type) *out_type = type_int();
      return true;
    }
    case Expr::Kind::FloatLiteral: {
      if (out_type) *out_type = type_float();
      return true;
    }
    case Expr::Kind::StringLiteral: {
      if (out_type) *out_type = type_string();
      return true;
    }
    case Expr::Kind::BoolLiteral:
    case Expr::Kind::NullLiteral: {
      if (out_type) *out_type = type_bool();
      return true;
    }
    case Expr::Kind::Ident: {
      if (e.ident == "it") {
        if (in_for_iter && for_it_type) {
          if (out_type) *out_type = for_it_type;
          return true;
        }
        add_error(e.loc, "'it' may only be used inside a for loop");
        return false;
      }
      if (e.ident == "it_index") {
        if (in_for_iter && for_it_index_type) {
          if (out_type) *out_type = for_it_index_type;
          return true;
        }
        add_error(e.loc, "'it_index' may only be used inside a for loop");
        return false;
      }
      if (e.ident == "print") {
        if (out_type) *out_type = type_int();
        return true;
      }
      Symbol* s = resolve_ident(e.ident);
      if (!s) {
        add_error(e.loc, "undefined identifier '" + e.ident + "'");
        return false;
      }
      if (out_type) {
        if (s->type) *out_type = s->type;
        else if (s->kind == Symbol::Kind::Const && s->const_int_value) *out_type = type_int();
      }
      return true;
    }
    case Expr::Kind::Binary: {
      std::shared_ptr<Type> lt, rt;
      if (!e.lhs || !e.rhs) return false;
      if (!check_expression(*e.lhs, &lt) || !check_expression(*e.rhs, &rt))
        return false;
      if (lt && rt && !lt->is_same(*rt)) {
        auto is_int_kind = [](const Type* t) {
          return t && (t->kind == TypeKind::Int || t->kind == TypeKind::Int8 ||
                       t->kind == TypeKind::Int16 || t->kind == TypeKind::Int32 ||
                       t->kind == TypeKind::Int64 || t->kind == TypeKind::UInt8 ||
                       t->kind == TypeKind::UInt16 || t->kind == TypeKind::UInt32 ||
                       t->kind == TypeKind::UInt64);
        };
        if (e.op == TokenKind::Eq && lt->is_assignable_from(*rt)) {
          if (out_type) *out_type = lt;
          return true;
        }
        if (e.op != TokenKind::Eq && is_int_kind(lt.get()) && is_int_kind(rt.get())) {
          if (out_type) *out_type = lt;
          return true;
        }
        if (e.op == TokenKind::Eq)
          add_error(e.loc, "assignment type mismatch (no implicit conversion)");
        else
          add_error(e.loc, "type mismatch in binary expression (no implicit conversion)");
        return false;
      }
      if (out_type) *out_type = lt ? lt : rt;
      return true;
    }
    case Expr::Kind::Call: {
      if (!e.lhs) return false;
      std::shared_ptr<Type> fn_type;
      if (!check_expression(*e.lhs, &fn_type)) return false;
      if (fn_type && fn_type->kind == TypeKind::Procedure && fn_type->proc_type) {
        auto* pt = fn_type->proc_type;
        if (pt->param_types.size() != e.args.size()) {
          add_error(e.loc, "argument count mismatch");
          return false;
        }
        for (size_t i = 0; i < e.args.size(); ++i) {
          if (!e.args[i]) continue;
          std::shared_ptr<Type> at;
          if (!check_expression(*e.args[i], &at)) return false;
          if (i < pt->param_types.size() && at && pt->param_types[i] &&
              !pt->param_types[i]->is_assignable_from(*at)) {
            add_error(e.loc, "argument type mismatch");
            return false;
          }
        }
        if (out_type) *out_type = pt->return_type ? pt->return_type : type_int();
      } else if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "print") {
        for (size_t i = 0; i < e.args.size(); ++i)
          if (e.args[i] && !check_expression(*e.args[i], nullptr)) return false;
        if (out_type) *out_type = type_int();
      } else if (e.lhs->kind == Expr::Kind::Ident && e.lhs->ident == "cast") {
        if (e.args.size() != 1 || !e.args[0]) {
          add_error(e.loc, "cast expects one argument");
          return false;
        }
        if (!e.cast_type && e.lhs->cast_type) { /* cast is (type) expr */ }
        std::shared_ptr<Type> at;
        if (!check_expression(*e.args[0], &at)) return false;
        if (out_type) *out_type = at;
      } else {
        if (out_type) *out_type = type_int();
      }
      return true;
    }
    case Expr::Kind::Index: {
      if (!e.lhs || !e.index_expr) return false;
      std::shared_ptr<Type> base_type;
      if (!check_expression(*e.lhs, &base_type)) return false;
      if (!check_expression(*e.index_expr, nullptr)) return false;
      if (base_type && base_type->kind == TypeKind::TypeTableView) {
        if (out_type) *out_type = type_type_info();
      } else if (base_type && base_type->element_type) {
        if (out_type) *out_type = base_type->element_type;
      } else {
        if (out_type) *out_type = type_int();
      }
      return true;
    }
    case Expr::Kind::Member: {
      if (!e.lhs) return false;
      std::shared_ptr<Type> base_type;
      if (!check_expression(*e.lhs, &base_type)) return false;
      if (base_type && base_type->struct_type) {
        int idx = base_type->struct_type->member_index(e.member_name);
        if (idx < 0) {
          add_error(e.loc, "no member '" + e.member_name + "'");
          return false;
        }
        if (out_type) *out_type = base_type->struct_type->members[idx].type;
      } else if (base_type && base_type->kind == TypeKind::ArrayDynamic &&
                 e.member_name == "count") {
        if (out_type) *out_type = type_int();
      } else if (base_type && base_type->kind == TypeKind::TypeTableView) {
        if (e.member_name == "count") {
          if (out_type) *out_type = type_int();
        } else if (e.member_name == "data") {
          auto elem = type_type_info();
          auto ptr = std::make_shared<Type>();
          ptr->kind = TypeKind::Pointer;
          ptr->element_type = elem;
          if (out_type) *out_type = ptr;
        } else {
          add_error(e.loc, "no member '" + e.member_name + "' on type table");
          return false;
        }
      } else if (base_type && base_type->kind == TypeKind::TypeInfo) {
        if (e.member_name == "name") {
          if (out_type) *out_type = type_string();
        } else if (e.member_name == "type") {
          if (out_type) *out_type = type_type_info_tag();
        } else {
          add_error(e.loc, "no member '" + e.member_name + "' on Type_Info");
          return false;
        }
      } else {
        add_error(e.loc, "member access on non-struct/array");
        return false;
      }
      return true;
    }
    case Expr::Kind::Cast: {
      if (!e.operand || !e.cast_type) return false;
      if (!check_expression(*e.operand, nullptr)) return false;
      std::shared_ptr<Type> to_type = resolve_type_expr(*e.cast_type);
      if (!to_type) {
        add_error(e.loc, "invalid cast type");
        return false;
      }
      if (out_type) *out_type = to_type;
      return true;
    }
    case Expr::Kind::Run:
      if (e.run_expr && !check_expression(*e.run_expr, out_type)) return false;
      else if (out_type) *out_type = type_int();
      return true;
    case Expr::Kind::InitList:
      if (out_type) *out_type = type_int();
      return true;
    default:
      if (out_type) *out_type = type_int();
      return true;
  }
}

bool SemaContext::check_statement(Stmt& s) {
  switch (s.kind) {
    case Stmt::Kind::Block: {
      Scope block(scope);
      scope = &block;
      for (auto& st : s.block_body)
        if (st && !check_statement(*st)) { scope = block.parent(); return false; }
      scope = block.parent();
      return true;
    }
    case Stmt::Kind::Expr:
      return s.expr && check_expression(*s.expr, nullptr);
    case Stmt::Kind::VarDecl: {
      std::shared_ptr<Type> t;
      if (s.var_type) {
        t = resolve_type_expr(*s.var_type);
        if (!t) {
          add_error(s.loc, "invalid type");
          return false;
        }
      }
      if (s.var_init && !check_expression(*s.var_init, nullptr)) return false;
      if (s.var_init && t) {
        std::shared_ptr<Type> it;
        if (check_expression(*s.var_init, &it) && it && !t->is_assignable_from(*it)) {
          add_error(s.loc, "initializer type mismatch (no implicit conversion)");
          return false;
        }
      }
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Var;
        sym.name = s.var_name;
        sym.type = t;
        scope->add(s.var_name, std::move(sym));
      }
      return true;
    }
    case Stmt::Kind::If:
      if (!s.if_cond || !check_expression(*s.if_cond, nullptr)) return false;
      if (!s.if_then || !check_statement(*s.if_then)) return false;
      if (s.if_else && !check_statement(*s.if_else)) return false;
      return true;
    case Stmt::Kind::ForRange: {
      if (!scope) return true;
      Symbol sym;
      sym.kind = Symbol::Kind::Var;
      sym.name = s.for_var_name;
      sym.type = type_int();
      scope->add(s.for_var_name, std::move(sym));
      if (s.for_range_start && !check_expression(*s.for_range_start, nullptr))
        return false;
      if (s.for_range_end && !check_expression(*s.for_range_end, nullptr))
        return false;
      if (s.for_body && !check_statement(*s.for_body)) return false;
      return true;
    }
    case Stmt::Kind::ForIter: {
      if (!s.for_iter_expr || !s.for_body) return false;
      std::shared_ptr<Type> iter_type;
      if (!check_expression(*s.for_iter_expr, &iter_type)) return false;
      bool was_in_for = in_for_iter;
      in_for_iter = true;
      for_it_index_type = type_int();
      if (iter_type && iter_type->kind == TypeKind::TypeTableView)
        for_it_type = type_type_info();
      else if (s.for_iter_expr->kind == Expr::Kind::Ident && s.for_iter_expr->ident == "_type_table")
        for_it_type = type_type_info();
      else if (iter_type && iter_type->element_type)
        for_it_type = iter_type->element_type;
      else
        for_it_type = type_int();
      bool ok = check_statement(*s.for_body);
      in_for_iter = was_in_for;
      for_it_type.reset();
      for_it_index_type.reset();
      return ok;
    }
    case Stmt::Kind::Loop:
      if (!s.loop_cond || !check_expression(*s.loop_cond, nullptr))
        return false;
      return s.loop_body && check_statement(*s.loop_body);
    case Stmt::Kind::Return:
      if (s.return_expr && !check_expression(*s.return_expr, nullptr))
        return false;
      return true;
    case Stmt::Kind::Defer:
      return s.defer_body && check_statement(*s.defer_body);
    default:
      return true;
  }
}

bool SemaContext::check_declaration(Decl& d, bool register_proc_only) {
  switch (d.kind) {
    case Decl::Kind::Var: {
      std::shared_ptr<Type> t;
      if (d.var_type) {
        t = resolve_type_expr(*d.var_type);
        if (!t) {
          add_error(d.loc, "invalid type");
          return false;
        }
      }
      if (d.var_init && !check_expression(*d.var_init, nullptr)) return false;
      if (d.var_init && t && d.var_init->kind != Expr::Kind::Run) {
        std::shared_ptr<Type> it;
        if (check_expression(*d.var_init, &it) && it && !t->is_assignable_from(*it)) {
          add_error(d.loc, "initializer type mismatch");
          return false;
        }
      }
      if (d.var_infer_type && d.var_init) {
        std::shared_ptr<Type> it;
        if (check_expression(*d.var_init, &it)) t = it;
      }
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Var;
        sym.name = d.var_name;
        sym.type = t;
        scope->add(d.var_name, std::move(sym));
      }
      return true;
    }
    case Decl::Kind::Const: {
      if (!d.var_init || !check_expression(*d.var_init, nullptr)) return false;
      std::shared_ptr<Type> t;
      if (!check_expression(*d.var_init, &t)) return false;
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Const;
        sym.name = d.var_name;
        sym.type = t;
        sym.decl = &d;
        if (d.var_init->kind == Expr::Kind::IntLiteral)
          sym.const_int_value = d.var_init->int_val;
        else if (d.var_init->kind == Expr::Kind::Ident) {
          Symbol* inner = resolve_ident(d.var_init->ident);
          if (inner && inner->kind == Symbol::Kind::Const && inner->const_int_value)
            sym.const_int_value = *inner->const_int_value;
        } else {
          auto opt = try_eval_int_const(*d.var_init);
          if (opt) sym.const_int_value = *opt;
        }
        scope->add(d.var_name, std::move(sym));
      }
      return true;
    }
    case Decl::Kind::Struct: {
      auto st = std::make_unique<StructType>();
      st->name = d.struct_name;
      st->soa = d.struct_soa;
      StructType* stp = st.get();
      structs_.push_back(std::move(st));
      auto t = std::make_shared<Type>();
      t->kind = stp->soa ? TypeKind::StructSOA : TypeKind::Struct;
      t->name = stp->name;
      t->struct_type = stp;
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Struct;
        sym.name = d.struct_name;
        sym.struct_type = stp;
        scope->add(d.struct_name, std::move(sym));
      }
      for (auto& m : d.struct_members) {
        StructType::Member mem;
        mem.name = m.name;
        mem.type = resolve_type_expr(*m.type);
        if (!mem.type) return false;
        mem.no_init = m.no_init;
        stp->members.push_back(std::move(mem));
      }
      return true;
    }
    case Decl::Kind::Enum: {
      auto et = std::make_unique<EnumType>();
      et->name = d.enum_name;
      et->backing_type = d.enum_backing_type ? resolve_type_expr(*d.enum_backing_type)
                                             : type_int();
      int64_t next_val = 0;
      for (auto& m : d.enum_members) {
        et->names.push_back(m.name);
        if (m.value) {
          std::shared_ptr<Type> vt;
          check_expression(*m.value, &vt);
          next_val = m.value->kind == Expr::Kind::IntLiteral ? m.value->int_val
                                                             : next_val;
        }
        et->values.push_back(next_val++);
      }
      EnumType* etp = et.get();
      enums_.push_back(std::move(et));
      auto t = std::make_shared<Type>();
      t->kind = TypeKind::Enum;
      t->name = etp->name;
      t->enum_type = etp;
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Enum;
        sym.name = d.enum_name;
        sym.enum_type = etp;
        scope->add(d.enum_name, std::move(sym));
      }
      return true;
    }
    case Decl::Kind::Proc: {
      if (!d.proc) return false;
      ProcDecl& p = *d.proc;
      if (register_proc_only) {
        auto pt = std::make_unique<ProcType>();
        pt->name = p.name;
        for (auto& param : p.params) {
          if (param.type)
            pt->param_types.push_back(resolve_type_expr(*param.type));
          else
            pt->param_types.push_back(nullptr);
        }
        pt->return_type = p.return_type ? resolve_type_expr(*p.return_type) : type_int();
        ProcType* ptp = pt.get();
        procs_.push_back(std::move(pt));
        auto fn_type = std::make_shared<Type>();
        fn_type->kind = TypeKind::Procedure;
        fn_type->name = p.name;
        fn_type->proc_type = ptp;
        if (scope) {
          Symbol sym;
          sym.kind = Symbol::Kind::Proc;
          sym.name = p.name;
          sym.type = fn_type;
          sym.proc_decl = &p;
          scope->add(p.name, std::move(sym));
        }
        return true;
      }
      Symbol* existing = scope ? scope->find(p.name) : nullptr;
      ProcType* ptp = nullptr;
      if (existing && existing->kind == Symbol::Kind::Proc && existing->proc_decl == &p && existing->type && existing->type->proc_type)
        ptp = existing->type->proc_type;
      if (!ptp) {
        add_error(p.loc, "procedure '" + p.name + "' not registered (sema two-pass)");
        return false;
      }
      Scope func_scope(scope);
      scope = &func_scope;
      for (size_t i = 0; i < p.params.size(); ++i) {
        Symbol psym;
        psym.kind = Symbol::Kind::Var;
        psym.name = p.params[i].name;
        psym.type = i < ptp->param_types.size() ? ptp->param_types[i] : nullptr;
        scope->add(p.params[i].name, std::move(psym));
      }
      bool ok = p.body && check_statement(*p.body);
      scope = func_scope.parent();
      return ok;
    }
    case Decl::Kind::DirectiveRun:
      if (d.directive_run_expr)
        return check_expression(*d.directive_run_expr, nullptr);
      return true;
    case Decl::Kind::DirectiveLoad:
    case Decl::Kind::DirectiveInline:
    case Decl::Kind::DirectiveNoInline:
      return true;
    case Decl::Kind::DirectiveExtern: {
      if (!d.proc) {
        // Extern variable: @extern name: Type;
        if (!register_proc_only) return true;  // already registered in pass 1
        if (!d.var_type) return false;
        std::shared_ptr<Type> t = resolve_type_expr(*d.var_type);
        if (!t) {
          add_error(d.loc, "invalid type for @extern variable");
          return false;
        }
        if (scope) {
          Symbol sym;
          sym.kind = Symbol::Kind::Var;
          sym.name = d.var_name;
          sym.type = t;
          scope->add(d.var_name, std::move(sym));
        }
        return true;
      }
      if (!register_proc_only) return true;  // already registered in pass 1
      ProcDecl& p = *d.proc;
      auto pt = std::make_unique<ProcType>();
      pt->name = p.name;
      for (auto& param : p.params) {
        if (param.type)
          pt->param_types.push_back(resolve_type_expr(*param.type));
        else
          pt->param_types.push_back(nullptr);
      }
      pt->return_type = p.return_type ? resolve_type_expr(*p.return_type) : type_int();
      ProcType* ptp = pt.get();
      procs_.push_back(std::move(pt));
      auto fn_type = std::make_shared<Type>();
      fn_type->kind = TypeKind::Procedure;
      fn_type->name = p.name;
      fn_type->proc_type = ptp;
      if (scope) {
        Symbol sym;
        sym.kind = Symbol::Kind::Proc;
        sym.name = p.name;
        sym.type = fn_type;
        sym.proc_decl = &p;
        scope->add(p.name, std::move(sym));
      }
      return true;
    }
    default:
      return true;
  }
}

bool SemaContext::check_file(File& f) {
  file = &f;
  file_scope_ = Scope(nullptr);
  scope = &file_scope_;
  structs_.clear();
  enums_.clear();
  procs_.clear();
  {
    Symbol type_table_sym;
    type_table_sym.kind = Symbol::Kind::Var;
    type_table_sym.name = "_type_table";
    type_table_sym.type = type_type_table();
    file_scope_.add("_type_table", std::move(type_table_sym));
  }
  // Pass 1: register all procedures and extern declarations first so they can be referenced out of order.
  for (auto& d : f.declarations) {
    if (d && (d->kind == Decl::Kind::Proc || d->kind == Decl::Kind::DirectiveExtern) && !check_declaration(*d, true)) {}
  }
  // Pass 2: full type-check all declarations (including procedure bodies).
  for (auto& d : f.declarations) {
    if (d && !check_declaration(*d, false)) {}
  }
  scope = &file_scope_;  // leave file scope so codegen can resolve constants/globals
  return errors.empty();
}

}  // namespace jai
