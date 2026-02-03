#include "ast_dump.hpp"
#include <iostream>

namespace jai {

static void indent(std::ostream& out, int n) {
  for (int i = 0; i < n; ++i) out << "  ";
}

void dump_type_expr(std::ostream& out, const TypeExpr& t, int indent_level) {
  indent(out, indent_level);
  switch (t.kind) {
    case TypeExpr::Kind::Ident:
      out << "TypeIdent(" << t.name << (t.type_param ? " $)" : ")");
      break;
    case TypeExpr::Kind::Pointer:
      out << "TypePointer\n";
      if (t.element_type) dump_type_expr(out, *t.element_type, indent_level + 1);
      break;
    case TypeExpr::Kind::OwnedPointer:
      out << "TypeOwnedPointer\n";
      if (t.element_type) dump_type_expr(out, *t.element_type, indent_level + 1);
      break;
    case TypeExpr::Kind::ArrayFixed:
      out << "TypeArrayFixed(" << t.fixed_count << ")\n";
      if (t.element_type) dump_type_expr(out, *t.element_type, indent_level + 1);
      break;
    case TypeExpr::Kind::ArrayDynamic:
      out << "TypeArrayDynamic\n";
      if (t.element_type) dump_type_expr(out, *t.element_type, indent_level + 1);
      break;
    case TypeExpr::Kind::Optional:
      out << "TypeOptional(" << t.name << ")";
      break;
  }
}

void dump_expr(std::ostream& out, const Expr& e, int indent_level) {
  indent(out, indent_level);
  switch (e.kind) {
    case Expr::Kind::IntLiteral:
      out << "IntLiteral(" << e.int_val << ")\n";
      break;
    case Expr::Kind::FloatLiteral:
      out << "FloatLiteral(" << e.float_val << ")\n";
      break;
    case Expr::Kind::StringLiteral:
      out << "StringLiteral(\"" << e.string_val << "\")\n";
      break;
    case Expr::Kind::BoolLiteral:
      out << "BoolLiteral(" << (e.bool_val ? "true" : "false") << ")\n";
      break;
    case Expr::Kind::NullLiteral:
      out << "NullLiteral\n";
      break;
    case Expr::Kind::Ident:
      out << "Ident(" << e.ident << ")\n";
      break;
    case Expr::Kind::Binary:
      out << "Binary\n";
      if (e.lhs) dump_expr(out, *e.lhs, indent_level + 1);
      indent(out, indent_level + 1);
      out << "op\n";
      if (e.rhs) dump_expr(out, *e.rhs, indent_level + 1);
      break;
    case Expr::Kind::Unary:
      out << "Unary\n";
      if (e.operand) dump_expr(out, *e.operand, indent_level + 1);
      break;
    case Expr::Kind::Call:
      out << "Call\n";
      if (e.lhs) dump_expr(out, *e.lhs, indent_level + 1);
      for (const auto& a : e.args)
        if (a) dump_expr(out, *a, indent_level + 1);
      break;
    case Expr::Kind::Index:
      out << "Index\n";
      if (e.lhs) dump_expr(out, *e.lhs, indent_level + 1);
      if (e.index_expr) dump_expr(out, *e.index_expr, indent_level + 1);
      break;
    case Expr::Kind::Member:
      out << "Member(" << e.member_name << ")\n";
      if (e.lhs) dump_expr(out, *e.lhs, indent_level + 1);
      break;
    case Expr::Kind::Cast:
      out << "Cast\n";
      if (e.cast_type) dump_type_expr(out, *e.cast_type, indent_level + 1);
      if (e.operand) dump_expr(out, *e.operand, indent_level + 1);
      break;
    case Expr::Kind::Run:
      out << "Run\n";
      if (e.run_expr) dump_expr(out, *e.run_expr, indent_level + 1);
      break;
    case Expr::Kind::InitList:
      out << "InitList\n";
      for (const auto& elem : e.init_list)
        if (elem) dump_expr(out, *elem, indent_level + 1);
      break;
    default:
      out << "Expr(unknown)\n";
  }
}

void dump_stmt(std::ostream& out, const Stmt& s, int indent_level) {
  indent(out, indent_level);
  switch (s.kind) {
    case Stmt::Kind::Block:
      out << "Block\n";
      for (const auto& c : s.block_body)
        if (c) dump_stmt(out, *c, indent_level + 1);
      break;
    case Stmt::Kind::Expr:
      out << "ExprStmt\n";
      if (s.expr) dump_expr(out, *s.expr, indent_level + 1);
      break;
    case Stmt::Kind::VarDecl:
      out << "VarDecl(" << s.var_name << ")\n";
      if (s.var_type) dump_type_expr(out, *s.var_type, indent_level + 1);
      if (s.var_init) dump_expr(out, *s.var_init, indent_level + 1);
      break;
    case Stmt::Kind::If:
      out << "If\n";
      if (s.if_cond) dump_expr(out, *s.if_cond, indent_level + 1);
      if (s.if_then) dump_stmt(out, *s.if_then, indent_level + 1);
      if (s.if_else) dump_stmt(out, *s.if_else, indent_level + 1);
      break;
    case Stmt::Kind::ForRange:
      out << "ForRange(" << s.for_var_name << ")\n";
      if (s.for_range_start) dump_expr(out, *s.for_range_start, indent_level + 1);
      if (s.for_range_end) dump_expr(out, *s.for_range_end, indent_level + 1);
      if (s.for_body) dump_stmt(out, *s.for_body, indent_level + 1);
      break;
    case Stmt::Kind::ForIter:
      out << "ForIter\n";
      if (s.for_iter_expr) dump_expr(out, *s.for_iter_expr, indent_level + 1);
      if (s.for_body) dump_stmt(out, *s.for_body, indent_level + 1);
      break;
    case Stmt::Kind::While:
      out << "While\n";
      if (s.while_cond) dump_expr(out, *s.while_cond, indent_level + 1);
      if (s.while_body) dump_stmt(out, *s.while_body, indent_level + 1);
      break;
    case Stmt::Kind::Return:
      out << "Return\n";
      if (s.return_expr) dump_expr(out, *s.return_expr, indent_level + 1);
      break;
    case Stmt::Kind::Defer:
      out << "Defer\n";
      if (s.defer_body) dump_stmt(out, *s.defer_body, indent_level + 1);
      break;
    default:
      out << "Stmt(unknown)\n";
  }
}

void dump_decl(std::ostream& out, const Decl& d, int indent_level) {
  indent(out, indent_level);
  switch (d.kind) {
    case Decl::Kind::Var:
      out << "VarDecl(" << d.var_name << ")\n";
      if (d.var_type) dump_type_expr(out, *d.var_type, indent_level + 1);
      if (d.var_init) dump_expr(out, *d.var_init, indent_level + 1);
      break;
    case Decl::Kind::Const:
      out << "ConstDecl(" << d.var_name << ")\n";
      if (d.var_init) dump_expr(out, *d.var_init, indent_level + 1);
      break;
    case Decl::Kind::Proc:
      out << "ProcDecl(" << (d.proc ? d.proc->name : "") << ")\n";
      if (d.proc && d.proc->body) dump_stmt(out, *d.proc->body, indent_level + 1);
      break;
    case Decl::Kind::Struct:
      out << "StructDecl(" << d.struct_name << (d.struct_soa ? " SOA" : "") << ")\n";
      break;
    case Decl::Kind::Enum:
      out << "EnumDecl(" << d.enum_name << ")\n";
      break;
    case Decl::Kind::DirectiveRun:
      out << "#run\n";
      if (d.directive_run_expr) dump_expr(out, *d.directive_run_expr, indent_level + 1);
      break;
    case Decl::Kind::DirectiveLoad:
      out << "#load \"" << d.directive_load_path << "\"\n";
      break;
    case Decl::Kind::DirectiveInline:
      out << "#inline " << d.directive_inline_proc << "\n";
      break;
    case Decl::Kind::DirectiveNoInline:
      out << "#no_inline " << d.directive_inline_proc << "\n";
      break;
    case Decl::Kind::DirectiveExtern:
      if (d.proc) {
        out << "@extern " << d.proc->name << "(...) -> ...\n";
      }
      break;
    default:
      out << "Decl(unknown)\n";
  }
}

void dump_file(std::ostream& out, const File& f) {
  out << "File: " << f.path << "\n";
  for (const auto& d : f.declarations)
    if (d) dump_decl(out, *d, 0);
}

}  // namespace jai
