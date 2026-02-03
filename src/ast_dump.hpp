#pragma once

#include "ast.hpp"
#include <ostream>

namespace jai {

void dump_type_expr(std::ostream& out, const TypeExpr& t, int indent = 0);
void dump_expr(std::ostream& out, const Expr& e, int indent = 0);
void dump_stmt(std::ostream& out, const Stmt& s, int indent = 0);
void dump_decl(std::ostream& out, const Decl& d, int indent = 0);
void dump_file(std::ostream& out, const File& f);

}  // namespace jai
