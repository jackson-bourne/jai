#pragma once

#include "ast.hpp"
#include <memory>
#include <string>
#include <vector>

namespace jai {

struct Type;
struct StructType;
struct EnumType;
struct ProcType;

enum class TypeKind {
  Invalid,
  Int,
  Int8,
  Int16,
  Int32,
  Int64,
  UInt8,
  UInt16,
  UInt32,
  UInt64,
  Float,
  Double,
  Bool,
  String,
  Pointer,
  OwnedPointer,
  ArrayFixed,
  ArrayDynamic,
  Struct,
  StructSOA,
  Enum,
  Procedure,
  Any,
  TypeInfo,      // reflection: Type_Info struct (tag, name)
  TypeInfoTag,   // reflection: Type_Info_Tag enum
  TypeTableView, // reflection: _type_table (data, count)
};

struct Type {
  TypeKind kind = TypeKind::Invalid;
  std::string name;  // for struct/enum/procedure

  std::shared_ptr<Type> element_type;  // *T, [N]T, [..]T
  int fixed_count = 0;

  StructType* struct_type = nullptr;
  EnumType* enum_type = nullptr;
  ProcType* proc_type = nullptr;

  bool is_same(const Type& other) const;
  bool is_assignable_from(const Type& other) const;
};

struct StructType {
  std::string name;
  bool soa = false;
  struct Member {
    std::string name;
    std::shared_ptr<Type> type;
    std::unique_ptr<Expr> default_init;
    bool no_init = false;
  };
  std::vector<Member> members;
  int member_index(const std::string& name) const;
};

struct EnumType {
  std::string name;
  std::shared_ptr<Type> backing_type;
  std::vector<std::string> names;
  std::vector<int64_t> values;
  int member_index(const std::string& name) const;
};

struct ProcType {
  std::string name;  // procedure name for reflection / type table
  std::vector<std::shared_ptr<Type>> param_types;
  std::shared_ptr<Type> return_type;
};

// Built-in type singletons
std::shared_ptr<Type> type_int();
std::shared_ptr<Type> type_float();
std::shared_ptr<Type> type_bool();
std::shared_ptr<Type> type_string();
std::shared_ptr<Type> type_any();
std::shared_ptr<Type> type_type_info();
std::shared_ptr<Type> type_type_info_tag();
std::shared_ptr<Type> type_type_table();

}  // namespace jai
