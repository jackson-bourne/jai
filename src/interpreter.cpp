#include "interpreter.hpp"
#include "build_config.hpp"
#include <cmath>
#include <iostream>
#include <sstream>
#include <stdexcept>

namespace jai {

namespace {

static void unescape_string(std::string& s) {
  if (s.size() >= 2 && s.front() == '"' && s.back() == '"')
    s = s.substr(1, s.size() - 2);
  std::string out;
  for (size_t i = 0; i < s.size(); ++i) {
    if (s[i] == '\\' && i + 1 < s.size()) {
      switch (s[i + 1]) {
        case 'n': out += '\n'; ++i; break;
        case 't': out += '\t'; ++i; break;
        case 'r': out += '\r'; ++i; break;
        case '\\': out += '\\'; ++i; break;
        case '"': out += '"'; ++i; break;
        default: out += s[i]; break;
      }
    } else
      out += s[i];
  }
  s = std::move(out);
}

// Format compile-time print args like printf so output matches runtime.
void compile_time_print(std::string fmt, const std::vector<RunValue>& args) {
  unescape_string(fmt);
  std::ostringstream out;
  size_t arg_idx = 0;
  for (size_t i = 0; i < fmt.size(); ++i) {
    if (fmt[i] == '%' && i + 1 < fmt.size()) {
      ++i;
      if (fmt[i] == '%') {
        out << '%';
        continue;
      }
      if (arg_idx >= args.size()) continue;
      const RunValue& v = args[arg_idx++];
      if (fmt[i] == 'l' && i + 2 < fmt.size() && fmt[i+1] == 'l' && fmt[i+2] == 'd') {
        i += 2;
        out << (v.tag == RunValue::Tag::F64 ? static_cast<int64_t>(v.f64) : v.i64);
        continue;
      }
      switch (fmt[i]) {
        case 'd':
        case 'i':
          out << (v.tag == RunValue::Tag::F64 ? static_cast<int64_t>(v.f64) : v.i64);
          break;
        case 'f':
        case 'F':
          out << (v.tag == RunValue::Tag::I64 ? static_cast<double>(v.i64) : v.f64);
          break;
        case 's':
          out << (v.tag == RunValue::Tag::Str ? v.str_val : "");
          break;
        default:
          out << '%' << fmt[i];
          --arg_idx;
          break;
      }
      continue;
    }
    out << fmt[i];
  }
  std::cout << out.str();
  std::cout.flush();
}

}  // namespace

CompileTimeEval::CompileTimeEval(File* file, SemaContext* sema)
    : file_(file), sema_(sema) {
  if (file_)
    for (auto& d : file_->declarations)
      if (d && d->kind == Decl::Kind::Proc && d->proc)
        procs_[d->proc->name] = d->proc.get();
}

void CompileTimeEval::set_local(const std::string& name, RunValue v) {
  locals_[name] = std::move(v);
}

RunValue CompileTimeEval::get_local(const std::string& name) const {
  auto it = locals_.find(name);
  if (it != locals_.end()) return it->second;
  return RunValue{};
}

bool CompileTimeEval::eval_expr(Expr& e, RunValue& out) {
  switch (e.kind) {
    case Expr::Kind::IntLiteral:
      out.tag = RunValue::Tag::I64;
      out.i64 = e.int_val;
      return true;
    case Expr::Kind::FloatLiteral:
      out.tag = RunValue::Tag::F64;
      out.f64 = e.float_val;
      return true;
    case Expr::Kind::StringLiteral:
      out.tag = RunValue::Tag::Str;
      out.str_val = e.string_val;
      return true;
    case Expr::Kind::BoolLiteral:
      out.tag = RunValue::Tag::I64;
      out.i64 = e.bool_val ? 1 : 0;
      return true;
    case Expr::Kind::NullLiteral:
      out.tag = RunValue::Tag::None;
      return true;
    case Expr::Kind::Ident: {
      if (e.ident == "it_index") {
        auto v = get_local("it_index");
        out = v;
        return true;
      }
      if (e.ident == "it") {
        auto v = get_local("it");
        out = v;
        return true;
      }
      auto v = get_local(e.ident);
      if (v.tag == RunValue::Tag::None && sema_) {
        Symbol* sym = sema_->resolve_ident(e.ident);
        if (sym && sym->kind == Symbol::Kind::Const && sym->const_int_value) {
          out.tag = RunValue::Tag::I64;
          out.i64 = *sym->const_int_value;
          return true;
        }
      }
      out = v;
      return true;
    }
    case Expr::Kind::Binary: {
      if (!e.lhs || !e.rhs) return false;
      RunValue L, R;
      if (!eval_expr(*e.lhs, L) || !eval_expr(*e.rhs, R)) return false;
      if (L.tag == RunValue::Tag::F64 || R.tag == RunValue::Tag::F64) {
        double a = L.tag == RunValue::Tag::F64 ? L.f64 : static_cast<double>(L.i64);
        double b = R.tag == RunValue::Tag::F64 ? R.f64 : static_cast<double>(R.i64);
        out.tag = RunValue::Tag::F64;
        switch (e.op) {
          case TokenKind::Plus: out.f64 = a + b; return true;
          case TokenKind::Minus: out.f64 = a - b; return true;
          case TokenKind::Star: out.f64 = a * b; return true;
          case TokenKind::Slash: out.f64 = (b != 0) ? a / b : 0; return true;
          default: return false;
        }
      }
      out.tag = RunValue::Tag::I64;
      switch (e.op) {
        case TokenKind::Plus: out.i64 = L.i64 + R.i64; return true;
        case TokenKind::Minus: out.i64 = L.i64 - R.i64; return true;
        case TokenKind::Star: out.i64 = L.i64 * R.i64; return true;
        case TokenKind::Slash: out.i64 = R.i64 ? L.i64 / R.i64 : 0; return true;
        default: return false;
      }
    }
    case Expr::Kind::Call: {
      if (!e.lhs || e.lhs->kind != Expr::Kind::Ident) return false;
      std::string name = e.lhs->ident;
      if (name == "print") {
        std::vector<RunValue> print_args;
        for (size_t i = 0; i < e.args.size(); ++i) {
          if (!e.args[i]) continue;
          RunValue av;
          if (!eval_expr(*e.args[i], av)) return false;
          print_args.push_back(std::move(av));
        }
        if (!print_args.empty()) {
          const RunValue& first = print_args[0];
          if (first.tag == RunValue::Tag::Str) {
            std::vector<RunValue> values(print_args.begin() + 1, print_args.end());
            compile_time_print(first.str_val, values);
          } else {
            compile_time_print("%lld", print_args);
          }
        }
        out.tag = RunValue::Tag::I64;
        out.i64 = 0;
        return true;
      }
      if (name == "add_build_file") {
        if (e.args.size() >= 1 && e.args[0] && e.args[0]->kind == Expr::Kind::StringLiteral) {
          std::string path = e.args[0]->string_val;
          if (path.size() >= 2 && path.front() == '"' && path.back() == '"')
            path = path.substr(1, path.size() - 2);
          get_build_config().build_files.push_back(path);
        }
        out.tag = RunValue::Tag::None;
        return true;
      }
      if (name == "update_build_options") {
        out.tag = RunValue::Tag::None;
        return true;
      }
      if (name == "cast") {
        if (e.args.size() != 1 || !e.args[0]) return false;
        RunValue v;
        if (!eval_expr(*e.args[0], v)) return false;
        if (e.cast_type && e.cast_type->name == "float") {
          out.tag = RunValue::Tag::F64;
          out.f64 = v.tag == RunValue::Tag::F64 ? v.f64 : static_cast<double>(v.i64);
          return true;
        }
        out = v;
        return true;
      }
      auto it = procs_.find(name);
      if (it == procs_.end()) return false;
      ProcDecl* callee = it->second;
      CompileTimeEval nested(file_, sema_);
      for (size_t i = 0; i < callee->params.size() && i < e.args.size(); ++i) {
        if (!e.args[i]) continue;
        RunValue arg;
        if (!eval_expr(*e.args[i], arg)) return false;
        nested.set_local(callee->params[i].name, std::move(arg));
      }
      if (!nested.eval_proc(*callee, out)) return false;
      return true;
    }
    case Expr::Kind::Index: {
      if (!e.lhs || !e.index_expr) return false;
      RunValue base, idx;
      if (!eval_expr(*e.lhs, base) || !eval_expr(*e.index_expr, idx)) return false;
      if (base.tag == RunValue::Tag::ArrayF64 && idx.tag == RunValue::Tag::I64) {
        size_t i = static_cast<size_t>(idx.i64);
        if (i < base.array_f64.size()) {
          out.tag = RunValue::Tag::F64;
          out.f64 = base.array_f64[i];
          return true;
        }
      }
      if (base.tag == RunValue::Tag::ArrayI64 && idx.tag == RunValue::Tag::I64) {
        size_t i = static_cast<size_t>(idx.i64);
        if (i < base.array_i64.size()) {
          out.tag = RunValue::Tag::I64;
          out.i64 = base.array_i64[i];
          return true;
        }
      }
      return false;
    }
    case Expr::Kind::Member:
      if (e.member_name == "count" && e.lhs) {
        RunValue base;
        if (!eval_expr(*e.lhs, base)) return false;
        out.tag = RunValue::Tag::I64;
        out.i64 = base.tag == RunValue::Tag::ArrayF64 ? static_cast<int64_t>(base.array_f64.size())
                 : base.tag == RunValue::Tag::ArrayI64 ? static_cast<int64_t>(base.array_i64.size())
                                                      : 0;
        return true;
      }
      return false;
    case Expr::Kind::Cast:
      if (e.operand && e.cast_type) {
        RunValue v;
        if (!eval_expr(*e.operand, v)) return false;
        if (e.cast_type->name == "float") {
          out.tag = RunValue::Tag::F64;
          out.f64 = v.tag == RunValue::Tag::F64 ? v.f64 : static_cast<double>(v.i64);
          return true;
        }
        if (e.cast_type->name == "int") {
          out.tag = RunValue::Tag::I64;
          out.i64 = v.tag == RunValue::Tag::F64 ? static_cast<int64_t>(v.f64) : v.i64;
          return true;
        }
      }
      return false;
    default:
      return false;
  }
}

bool CompileTimeEval::eval_stmt(Stmt& s) {
  switch (s.kind) {
    case Stmt::Kind::Block:
      for (auto& st : s.block_body)
        if (st && eval_stmt(*st)) { if (has_return_) return true; }
      return true;
    case Stmt::Kind::Expr:
      if (s.expr) {
        if (s.expr->kind == Expr::Kind::Binary && s.expr->op == TokenKind::Eq &&
            s.expr->lhs && s.expr->rhs) {
          if (s.expr->lhs->kind == Expr::Kind::Index &&
              s.expr->lhs->lhs && s.expr->lhs->lhs->kind == Expr::Kind::Ident) {
            std::string base_name = s.expr->lhs->lhs->ident;
            RunValue base = get_local(base_name);
            RunValue idx, rhs;
            if (!eval_expr(*s.expr->lhs->index_expr, idx) || !eval_expr(*s.expr->rhs, rhs))
              return true;
            if (base.tag == RunValue::Tag::ArrayF64 && idx.tag == RunValue::Tag::I64) {
              size_t i = static_cast<size_t>(idx.i64);
              if (i < base.array_f64.size()) {
                base.array_f64[i] = rhs.tag == RunValue::Tag::F64 ? rhs.f64 : static_cast<double>(rhs.i64);
                set_local(base_name, std::move(base));
              }
            } else if (base.tag == RunValue::Tag::ArrayI64 && idx.tag == RunValue::Tag::I64) {
              size_t i = static_cast<size_t>(idx.i64);
              if (i < base.array_i64.size()) {
                base.array_i64[i] = rhs.tag == RunValue::Tag::I64 ? rhs.i64 : static_cast<int64_t>(rhs.f64);
                set_local(base_name, std::move(base));
              }
            }
            return true;
          }
          if (s.expr->lhs->kind == Expr::Kind::Ident) {
            RunValue rhs;
            if (eval_expr(*s.expr->rhs, rhs))
              set_local(s.expr->lhs->ident, std::move(rhs));
            return true;
          }
          if (s.expr->lhs->kind == Expr::Kind::Member && s.expr->lhs->lhs &&
              s.expr->lhs->lhs->kind == Expr::Kind::Ident &&
              s.expr->lhs->lhs->ident == "build_options") {
            RunValue rhs;
            if (!eval_expr(*s.expr->rhs, rhs)) return true;
            std::string member = s.expr->lhs->member_name;
            if (member == "executable_name") {
              if (rhs.tag == RunValue::Tag::Str)
                get_build_config().options.executable_name = rhs.str_val;
            } else if (member == "optimization_level") {
              get_build_config().options.optimization_level = static_cast<int>(rhs.i64);
            } else if (member == "emit_line_directives") {
              get_build_config().options.emit_line_directives = (rhs.i64 != 0);
            }
            return true;
          }
        }
        RunValue v;
        eval_expr(*s.expr, v);
      }
      return true;
    case Stmt::Kind::VarDecl: {
      RunValue v;
      if (s.var_init && eval_expr(*s.var_init, v))
        set_local(s.var_name, std::move(v));
      else if (s.var_type && s.var_type->kind == TypeExpr::Kind::ArrayFixed) {
        int count = s.var_type->fixed_count;
        if (count <= 0 && s.var_type->count_expr) {
          if (s.var_type->count_expr->kind == Expr::Kind::IntLiteral)
            count = static_cast<int>(s.var_type->count_expr->int_val);
          else {
            RunValue cv;
            if (eval_expr(*s.var_type->count_expr, cv) && cv.tag == RunValue::Tag::I64)
              count = static_cast<int>(cv.i64);
          }
        }
        if (count > 0) {
          v.tag = RunValue::Tag::ArrayF64;
          v.array_f64.resize(static_cast<size_t>(count), 0.0);
          set_local(s.var_name, std::move(v));
        }
      }
      return true;
    }
    case Stmt::Kind::ForRange: {
      if (!s.for_range_start || !s.for_range_end || !s.for_body) return false;
      RunValue start, end;
      if (!eval_expr(*s.for_range_start, start) || !eval_expr(*s.for_range_end, end))
        return false;
      int64_t lo = start.i64, hi = end.i64;
      for (int64_t i = lo; i <= hi; ++i) {
        RunValue iv;
        iv.tag = RunValue::Tag::I64;
        iv.i64 = i;
        set_local(s.for_var_name, std::move(iv));
        if (!eval_stmt(*s.for_body)) return false;
        if (has_return_) return true;
      }
      return true;
    }
    case Stmt::Kind::ForIter: {
      if (!s.for_iter_expr || !s.for_body) return false;
      RunValue arr;
      if (!eval_expr(*s.for_iter_expr, arr)) return false;
      std::string arr_name = (s.for_iter_expr->kind == Expr::Kind::Ident) ? s.for_iter_expr->ident : "";
      if (arr.tag == RunValue::Tag::ArrayF64) {
        for (size_t i = 0; i < arr.array_f64.size(); ++i) {
          RunValue idx; idx.tag = RunValue::Tag::I64; idx.i64 = static_cast<int64_t>(i);
          RunValue it; it.tag = RunValue::Tag::F64; it.f64 = arr.array_f64[i];
          set_local("it_index", std::move(idx));
          set_local("it", std::move(it));
          if (!eval_stmt(*s.for_body)) return false;
          if (has_return_) return true;
          if (!arr_name.empty()) arr = get_local(arr_name);
          if (arr.tag != RunValue::Tag::ArrayF64) break;
        }
      } else if (arr.tag == RunValue::Tag::ArrayI64) {
        for (size_t i = 0; i < arr.array_i64.size(); ++i) {
          RunValue idx; idx.tag = RunValue::Tag::I64; idx.i64 = static_cast<int64_t>(i);
          RunValue it; it.tag = RunValue::Tag::I64; it.i64 = arr.array_i64[i];
          set_local("it_index", std::move(idx));
          set_local("it", std::move(it));
          if (!eval_stmt(*s.for_body)) return false;
          if (has_return_) return true;
          if (!arr_name.empty()) arr = get_local(arr_name);
        }
      }
      return true;
    }
    case Stmt::Kind::Return:
      has_return_ = true;
      if (s.return_expr)
        return eval_expr(*s.return_expr, return_value_);
      return_value_.tag = RunValue::Tag::None;
      return true;
    default:
      return true;
  }
}

bool CompileTimeEval::eval_proc(ProcDecl& p, RunValue& result) {
  has_return_ = false;
  if (!p.body) return false;
  for (auto& st : p.body->block_body) {
    if (!st) continue;
    if (!eval_stmt(*st)) return false;
    if (has_return_) {
      result = return_value_;
      return true;
    }
  }
  result.tag = RunValue::Tag::None;
  return true;
}

RunValue evaluate_compile_time_function(const std::string& name, File* file, SemaContext* sema) {
  if (!file) return RunValue{};
  CompileTimeEval eval(file, sema);
  for (auto& d : file->declarations) {
    if (!d || d->kind != Decl::Kind::Proc || !d->proc) continue;
    if (d->proc->name == name) {
      RunValue result;
      if (eval.eval_proc(*d->proc, result)) return result;
      return RunValue{};
    }
  }
  return RunValue{};
}

RunValue evaluate_compile_time_expr(Expr& e, File* file, SemaContext* sema) {
  CompileTimeEval eval(file, sema);
  RunValue result;
  if (eval.eval_expr(e, result)) return result;
  return RunValue{};
}

}  // namespace jai
