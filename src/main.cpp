#include "ast_dump.hpp"
#include "build_config.hpp"
#include "codegen.hpp"
#include "interpreter.hpp"
#include "parser.hpp"
#include "sema.hpp"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <set>
#include <sstream>
#include <string>

static std::string read_file(const std::string& path) {
  std::ifstream f(path);
  if (!f) return {};
  std::stringstream buf;
  buf << f.rdbuf();
  return buf.str();
}

static std::string dir_of(const std::string& path) {
  std::string::size_type p = path.find_last_of("/\\");
  if (p == std::string::npos) return "";
  return path.substr(0, p);
}

static std::string resolve_path(const std::string& base_dir, const std::string& rel) {
  std::string path = rel;
  if (path.size() >= 2 && path.front() == '"' && path.back() == '"')
    path = path.substr(1, path.size() - 2);
  if (base_dir.empty()) return path;
  return base_dir + "/" + path;
}

static bool expand_loads(jai::File* file, const std::string& base_dir,
                        std::set<std::string>& loaded) {
  std::vector<std::unique_ptr<jai::Decl>> out;
  for (auto& d : file->declarations) {
    if (d && d->kind == jai::Decl::Kind::DirectiveLoad) {
      std::string path = resolve_path(base_dir, d->directive_load_path);
      if (loaded.count(path)) continue;
      loaded.insert(path);
      std::string source = read_file(path);
      if (source.empty()) {
        llvm::errs() << "jai: could not read #load \"" << path << "\"\n";
        return false;
      }
      jai::Parser sub_parser(std::move(source), path);
      std::unique_ptr<jai::File> sub = sub_parser.parse_file();
      if (sub_parser.has_errors()) {
        for (const auto& err : sub_parser.errors())
          llvm::errs() << err << "\n";
        return false;
      }
      std::string sub_base = dir_of(path);
      if (!expand_loads(sub.get(), sub_base, loaded)) return false;
      for (auto& sub_d : sub->declarations)
        out.push_back(std::move(sub_d));
      continue;
    }
    out.push_back(std::move(d));
  }
  file->declarations = std::move(out);
  return true;
}

static bool run_build_if_present(jai::File* file, jai::SemaContext* sema) {
  for (auto& d : file->declarations) {
    if (!d || d->kind != jai::Decl::Kind::DirectiveRun || !d->directive_run_expr) continue;
    jai::Expr* e = d->directive_run_expr.get();
    if (e->kind == jai::Expr::Kind::Call && e->lhs && e->lhs->kind == jai::Expr::Kind::Ident &&
        e->lhs->ident == "build") {
      jai::evaluate_compile_time_function("build", file, sema);
      return true;
    }
  }
  return false;
}

int main(int argc, char** argv) {
  bool dump_ast = false;
  std::string input_path;
  std::string output_path;

  for (int i = 1; i < argc; ++i) {
    std::string arg = argv[i];
    if (arg == "--dump-ast" || arg == "-d")
      dump_ast = true;
    else if (arg == "--help" || arg == "-h") {
      llvm::outs() << "Usage: jai [options] <file.jai> [-o <output>]\n"
                   << "  --dump-ast, -d    Dump AST and exit\n"
                   << "  -o <file>        Write executable to <file>\n"
                   << "  --help, -h        Show this help\n";
      return 0;
    } else if (arg == "-o" && i + 1 < argc) {
      output_path = argv[++i];
    } else if (!arg.empty() && arg[0] != '-') {
      input_path = arg;
    }
  }

  if (input_path.empty()) {
    llvm::errs() << "jai: no input file\n";
    return 1;
  }

  jai::build_config_clear();

  std::string source = read_file(input_path);
  if (source.empty()) {
    llvm::errs() << "jai: could not read " << input_path << "\n";
    return 1;
  }

  jai::Parser parser(std::move(source), input_path);
  std::unique_ptr<jai::File> file = parser.parse_file();

  if (parser.has_errors()) {
    for (const auto& err : parser.errors())
      llvm::errs() << err << "\n";
    return 1;
  }

  std::string entry_dir = dir_of(input_path);
  std::set<std::string> loaded;
  if (!expand_loads(file.get(), entry_dir, loaded)) return 1;

  jai::SemaContext sema;
  if (!sema.check_file(*file)) {
    for (const auto& err : sema.errors)
      llvm::errs() << err << "\n";
    return 1;
  }

  run_build_if_present(file.get(), &sema);

  for (const std::string& build_path : jai::get_build_config().build_files) {
    std::string path = resolve_path(entry_dir, build_path);
    if (loaded.count(path)) continue;
    loaded.insert(path);
    std::string sub_source = read_file(path);
    if (sub_source.empty()) {
      llvm::errs() << "jai: could not read add_build_file \"" << path << "\"\n";
      return 1;
    }
    jai::Parser sub_parser(std::move(sub_source), path);
    std::unique_ptr<jai::File> sub = sub_parser.parse_file();
    if (sub_parser.has_errors()) {
      for (const auto& err : sub_parser.errors())
        llvm::errs() << err << "\n";
      return 1;
    }
    std::string sub_base = dir_of(path);
    std::set<std::string> sub_loaded = loaded;
    if (!expand_loads(sub.get(), sub_base, sub_loaded)) return 1;
    loaded = std::move(sub_loaded);
    for (auto& sub_d : sub->declarations)
      file->declarations.push_back(std::move(sub_d));
  }

  if (!sema.check_file(*file)) {
    for (const auto& err : sema.errors)
      llvm::errs() << err << "\n";
    return 1;
  }

  if (dump_ast && file) {
    jai::dump_file(std::cout, *file);
    return 0;
  }

  if (!file) {
    llvm::errs() << "jai: parse failed\n";
    return 1;
  }

  std::string exe_name = output_path;
  if (exe_name.empty() && !jai::get_build_config().options.executable_name.empty())
    exe_name = jai::get_build_config().options.executable_name;

  jai::CodeGen cg("jai_module");
  if (!cg.run(*file, sema)) {
    llvm::errs() << "jai: codegen failed\n";
    return 1;
  }

  std::string ir_path = exe_name.empty() ? "output.ll" : exe_name + ".ll";
  if (!cg.write_ir_to_file(ir_path)) {
    llvm::errs() << "jai: could not write " << ir_path << "\n";
    return 1;
  }

  if (!exe_name.empty()) {
    std::string cmd = "clang " + ir_path + " -o " + exe_name;
    if (std::system(cmd.c_str()) != 0) {
      llvm::errs() << "jai: linking failed (run: clang " << ir_path << " -o " << exe_name << ")\n";
      return 1;
    }
    std::remove(ir_path.c_str());
    llvm::outs() << "jai: built " << exe_name << "\n";
  } else {
    llvm::outs() << "jai: wrote " << ir_path << " (use -o <exe> to build executable)\n";
  }
  return 0;
}
