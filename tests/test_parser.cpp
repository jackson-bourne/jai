#include <gtest/gtest.h>
#include "parser.hpp"
#include "ast.hpp"
#include <string>
#include <cstring>

namespace {

jai::File* parse_ok(const std::string& source) {
  jai::Parser p(source, "<test>");
  std::unique_ptr<jai::File> file = p.parse_file();
  EXPECT_FALSE(p.has_errors()) << "Parse errors: " << (p.errors().empty() ? "" : p.errors()[0]);
  return file.release();
}

}  // namespace

TEST(Parser, EmptyFile) {
  std::unique_ptr<jai::File> f(parse_ok(""));
  ASSERT_TRUE(f);
  EXPECT_TRUE(f->declarations.empty());
}

TEST(Parser, VarDeclTypeValue) {
  std::unique_ptr<jai::File> f(parse_ok("x: int = 0;"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Var);
  EXPECT_EQ(d->var_name, "x");
  ASSERT_TRUE(d->var_type);
  EXPECT_EQ(d->var_type->kind, jai::TypeExpr::Kind::Ident);
  EXPECT_EQ(d->var_type->name, "int");
  ASSERT_TRUE(d->var_init);
  EXPECT_EQ(d->var_init->kind, jai::Expr::Kind::IntLiteral);
  EXPECT_EQ(d->var_init->int_val, 0);
  EXPECT_FALSE(d->var_infer_type);
}

TEST(Parser, VarDeclInferType) {
  std::unique_ptr<jai::File> f(parse_ok("counter := 0;"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Var);
  EXPECT_EQ(d->var_name, "counter");
  EXPECT_TRUE(d->var_infer_type);
  ASSERT_TRUE(d->var_init);
  EXPECT_EQ(d->var_init->kind, jai::Expr::Kind::IntLiteral);
}

TEST(Parser, VarDeclNoInit) {
  std::unique_ptr<jai::File> f(parse_ok("name: string;"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Var);
  EXPECT_EQ(d->var_name, "name");
  ASSERT_TRUE(d->var_type);
  EXPECT_EQ(d->var_type->name, "string");
  EXPECT_FALSE(d->var_init);
}

TEST(Parser, ConstDecl) {
  std::unique_ptr<jai::File> f(parse_ok("N :: 42;"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Const);
  EXPECT_EQ(d->var_name, "N");
  ASSERT_TRUE(d->var_init);
  EXPECT_EQ(d->var_init->kind, jai::Expr::Kind::IntLiteral);
  EXPECT_EQ(d->var_init->int_val, 42);
}

TEST(Parser, ProcDecl) {
  std::unique_ptr<jai::File> f(parse_ok(
      "sum :: (x: int, y: int) -> int { return x + y; };"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Proc);
  ASSERT_TRUE(d->proc);
  EXPECT_EQ(d->proc->name, "sum");
  ASSERT_EQ(d->proc->params.size(), 2u);
  EXPECT_EQ(d->proc->params[0].name, "x");
  EXPECT_EQ(d->proc->params[0].type->name, "int");
  EXPECT_EQ(d->proc->params[1].name, "y");
  EXPECT_EQ(d->proc->params[1].type->name, "int");
  ASSERT_TRUE(d->proc->return_type);
  EXPECT_EQ(d->proc->return_type->name, "int");
  ASSERT_TRUE(d->proc->body);
  EXPECT_EQ(d->proc->body->kind, jai::Stmt::Kind::Block);
  ASSERT_EQ(d->proc->body->block_body.size(), 1u);
  EXPECT_EQ(d->proc->body->block_body[0]->kind, jai::Stmt::Kind::Return);
}

TEST(Parser, StructDecl) {
  std::unique_ptr<jai::File> f(parse_ok(
      "Vector3 :: struct { x: float; y: float; z: float; };"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Struct);
  EXPECT_EQ(d->struct_name, "Vector3");
  ASSERT_EQ(d->struct_members.size(), 3u);
  EXPECT_EQ(d->struct_members[0].name, "x");
  EXPECT_EQ(d->struct_members[0].type->name, "float");
  EXPECT_EQ(d->struct_members[1].name, "y");
  EXPECT_EQ(d->struct_members[2].name, "z");
}

TEST(Parser, EnumDecl) {
  std::unique_ptr<jai::File> f(parse_ok(
      "E :: enum { A, B, C = 42 };"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::Enum);
  EXPECT_EQ(d->enum_name, "E");
  ASSERT_EQ(d->enum_members.size(), 3u);
  EXPECT_EQ(d->enum_members[0].name, "A");
  EXPECT_EQ(d->enum_members[1].name, "B");
  EXPECT_EQ(d->enum_members[2].name, "C");
  ASSERT_TRUE(d->enum_members[2].value);
  EXPECT_EQ(d->enum_members[2].value->kind, jai::Expr::Kind::IntLiteral);
  EXPECT_EQ(d->enum_members[2].value->int_val, 42);
}

TEST(Parser, DirectiveRun) {
  std::unique_ptr<jai::File> f(parse_ok("#run foo();"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::DirectiveRun);
  ASSERT_TRUE(d->directive_run_expr);
  EXPECT_EQ(d->directive_run_expr->kind, jai::Expr::Kind::Call);
  ASSERT_TRUE(d->directive_run_expr->lhs);
  EXPECT_EQ(d->directive_run_expr->lhs->ident, "foo");
}

TEST(Parser, DirectiveLoad) {
  std::unique_ptr<jai::File> f(parse_ok(R"(#load "other.jai";)"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Decl* d = f->declarations[0].get();
  ASSERT_EQ(d->kind, jai::Decl::Kind::DirectiveLoad);
  EXPECT_EQ(d->directive_load_path, "\"other.jai\"");
}

TEST(Parser, ForRange) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { for i : 0..9 { } };"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Stmt* body = f->declarations[0]->proc->body.get();
  ASSERT_EQ(body->block_body.size(), 1u);
  jai::Stmt* s = body->block_body[0].get();
  ASSERT_EQ(s->kind, jai::Stmt::Kind::ForRange);
  EXPECT_EQ(s->for_var_name, "i");
  ASSERT_TRUE(s->for_range_start);
  EXPECT_EQ(s->for_range_start->kind, jai::Expr::Kind::IntLiteral);
  EXPECT_EQ(s->for_range_start->int_val, 0);
  ASSERT_TRUE(s->for_range_end);
  EXPECT_EQ(s->for_range_end->kind, jai::Expr::Kind::IntLiteral);
  EXPECT_EQ(s->for_range_end->int_val, 9);
}

TEST(Parser, ForIter) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { for _type_table { it; } };"));
  ASSERT_TRUE(f);
  ASSERT_EQ(f->declarations.size(), 1u);
  jai::Stmt* body = f->declarations[0]->proc->body.get();
  ASSERT_GE(body->block_body.size(), 1u);
  jai::Stmt* s = body->block_body[0].get();
  ASSERT_EQ(s->kind, jai::Stmt::Kind::ForIter);
  ASSERT_TRUE(s->for_iter_expr);
  EXPECT_EQ(s->for_iter_expr->kind, jai::Expr::Kind::Ident);
  EXPECT_EQ(s->for_iter_expr->ident, "_type_table");
}

TEST(Parser, IfElse) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { if x then { } else { }; };"));
  ASSERT_TRUE(f);
  jai::Stmt* s = f->declarations[0]->proc->body->block_body[0].get();
  ASSERT_EQ(s->kind, jai::Stmt::Kind::If);
  EXPECT_TRUE(s->if_cond);
  EXPECT_TRUE(s->if_then);
  EXPECT_TRUE(s->if_else);
}

TEST(Parser, TypeExprPointer) {
  std::unique_ptr<jai::File> f(parse_ok("p: * int = null;"));
  ASSERT_TRUE(f);
  ASSERT_TRUE(f->declarations[0]->var_type);
  EXPECT_EQ(f->declarations[0]->var_type->kind, jai::TypeExpr::Kind::Pointer);
  EXPECT_EQ(f->declarations[0]->var_type->element_type->name, "int");
}

TEST(Parser, TypeExprArrayFixed) {
  std::unique_ptr<jai::File> f(parse_ok("a: [50] int;"));
  ASSERT_TRUE(f);
  jai::TypeExpr* t = f->declarations[0]->var_type.get();
  ASSERT_TRUE(t);
  EXPECT_EQ(t->kind, jai::TypeExpr::Kind::ArrayFixed);
  EXPECT_EQ(t->fixed_count, 50);
  ASSERT_TRUE(t->element_type);
  EXPECT_EQ(t->element_type->name, "int");
}

TEST(Parser, TypeExprArrayDynamic) {
  std::unique_ptr<jai::File> f(parse_ok("b: [..] int;"));
  ASSERT_TRUE(f);
  jai::TypeExpr* t = f->declarations[0]->var_type.get();
  ASSERT_TRUE(t);
  EXPECT_EQ(t->kind, jai::TypeExpr::Kind::ArrayDynamic);
  ASSERT_TRUE(t->element_type);
  EXPECT_EQ(t->element_type->name, "int");
}

TEST(Parser, MemberAccess) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { x.y; };"));
  ASSERT_TRUE(f);
  jai::Stmt* expr_stmt = f->declarations[0]->proc->body->block_body[0].get();
  ASSERT_EQ(expr_stmt->kind, jai::Stmt::Kind::Expr);
  jai::Expr* e = expr_stmt->expr.get();
  ASSERT_TRUE(e);
  EXPECT_EQ(e->kind, jai::Expr::Kind::Member);
  EXPECT_EQ(e->member_name, "y");
  ASSERT_TRUE(e->lhs);
  EXPECT_EQ(e->lhs->ident, "x");
}

TEST(Parser, Call) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { f(1, 2); };"));
  ASSERT_TRUE(f);
  jai::Expr* e = f->declarations[0]->proc->body->block_body[0]->expr.get();
  ASSERT_EQ(e->kind, jai::Expr::Kind::Call);
  ASSERT_TRUE(e->lhs);
  EXPECT_EQ(e->lhs->ident, "f");
  ASSERT_EQ(e->args.size(), 2u);
}

TEST(Parser, Cast) {
  std::unique_ptr<jai::File> f(parse_ok(
      "main :: () { cast(float) x; };"));
  ASSERT_TRUE(f);
  jai::Expr* e = f->declarations[0]->proc->body->block_body[0]->expr.get();
  ASSERT_EQ(e->kind, jai::Expr::Kind::Cast);
  ASSERT_TRUE(e->cast_type);
  EXPECT_EQ(e->cast_type->name, "float");
  ASSERT_TRUE(e->operand);
  EXPECT_EQ(e->operand->ident, "x");
}
