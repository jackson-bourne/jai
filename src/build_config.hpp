#pragma once

#include <string>
#include <vector>

namespace jai {

struct BuildOptions {
  std::string executable_name;
  int optimization_level = 0;  // 0=debug, 1=release, etc.
  bool emit_line_directives = true;
};

struct BuildConfig {
  BuildOptions options;
  std::vector<std::string> build_files;  // from add_build_file() and #load
};

BuildConfig& get_build_config();
void build_config_clear();

}  // namespace jai
