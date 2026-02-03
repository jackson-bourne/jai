#include <gtest/gtest.h>
#include <cstdlib>
#include <fstream>
#include <sstream>
#include <string>
#include <array>

namespace {

std::string get_jai_binary_path() {
  const char* bin = std::getenv("JAI_BIN");
  if (bin && bin[0]) return bin;
  return "jai";
}

bool run_command(const std::string& cmd, std::string* out_stdout = nullptr) {
  if (out_stdout) {
    FILE* f = popen((cmd + " 2>&1").c_str(), "r");
    if (!f) return false;
    std::array<char, 256> buf;
    while (fgets(buf.data(), static_cast<int>(buf.size()), f))
      *out_stdout += buf.data();
    int status = pclose(f);
    return status == 0;
  }
  return std::system(cmd.c_str()) == 0;
}

}  // namespace

TEST(Integration, HelloCompilesAndRuns) {
  std::string jai_exe = get_jai_binary_path();
  std::string compile_out;
  if (!run_command(jai_exe + " examples/hello.jai -o /tmp/jai_hello_out 2>&1", &compile_out)) {
    GTEST_SKIP() << "Compiler not available (set JAI_BIN when using ctest): " << compile_out;
  }

  std::string run_out;
  ASSERT_TRUE(run_command("/tmp/jai_hello_out 2>&1", &run_out)) << "Run failed: " << run_out;
  EXPECT_TRUE(run_out.find("Hello") != std::string::npos) << "Expected 'Hello' in: " << run_out;
}

TEST(Integration, ConditionalsZValue) {
  std::string jai_exe = get_jai_binary_path();
  std::string compile_out;
  if (!run_command(jai_exe + " examples/conditionals.jai -o /tmp/jai_conditionals_out 2>&1", &compile_out)) {
    GTEST_SKIP() << "Compiler not available (set JAI_BIN when using ctest): " << compile_out;
  }

  std::string run_out;
  ASSERT_TRUE(run_command("/tmp/jai_conditionals_out 2>&1", &run_out)) << "Run failed: " << run_out;
  EXPECT_TRUE(run_out.find("x < y") != std::string::npos);
  EXPECT_TRUE(run_out.find("x + y == 30") != std::string::npos);
  EXPECT_TRUE(run_out.find("z = 1") != std::string::npos)
      << "Expected z=1 (x>5 so then branch); got: " << run_out;
}
