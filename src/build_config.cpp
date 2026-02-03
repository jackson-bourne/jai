#include "build_config.hpp"

namespace jai {

namespace {

BuildConfig g_build_config;

}  // namespace

BuildConfig& get_build_config() {
  return g_build_config;
}

void build_config_clear() {
  g_build_config.build_files.clear();
  g_build_config.options = BuildOptions{};
}

}  // namespace jai
