#include "types.hpp"

namespace jai {

namespace {

std::shared_ptr<Type> g_int;
std::shared_ptr<Type> g_float;
std::shared_ptr<Type> g_bool;
std::shared_ptr<Type> g_string;
std::shared_ptr<Type> g_any;
std::shared_ptr<Type> g_type_info;
std::shared_ptr<Type> g_type_info_tag;
std::shared_ptr<Type> g_type_table;

}  // namespace

bool Type::is_same(const Type& other) const {
  if (kind != other.kind) return false;
  switch (kind) {
    case TypeKind::Invalid:
      return true;
    case TypeKind::Int:
    case TypeKind::Int8:
    case TypeKind::Int16:
    case TypeKind::Int32:
    case TypeKind::Int64:
    case TypeKind::UInt8:
    case TypeKind::UInt16:
    case TypeKind::UInt32:
    case TypeKind::UInt64:
    case TypeKind::Float:
    case TypeKind::Double:
    case TypeKind::Bool:
    case TypeKind::String:
    case TypeKind::Any:
      return true;
    case TypeKind::Pointer:
    case TypeKind::OwnedPointer:
      return element_type && other.element_type &&
             element_type->is_same(*other.element_type);
    case TypeKind::ArrayFixed:
      return fixed_count == other.fixed_count && element_type &&
             other.element_type && element_type->is_same(*other.element_type);
    case TypeKind::ArrayDynamic:
      return element_type && other.element_type &&
             element_type->is_same(*other.element_type);
    case TypeKind::Struct:
    case TypeKind::StructSOA:
      return struct_type == other.struct_type;
    case TypeKind::Enum:
      return enum_type == other.enum_type;
    case TypeKind::Procedure:
      return proc_type == other.proc_type;
    case TypeKind::TypeInfo:
    case TypeKind::TypeInfoTag:
    case TypeKind::TypeTableView:
      return true;
  }
  return false;
}

bool Type::is_assignable_from(const Type& other) const {
  if (kind == TypeKind::Any) return true;
  return is_same(other);
}

int StructType::member_index(const std::string& name) const {
  for (size_t i = 0; i < members.size(); ++i)
    if (members[i].name == name) return static_cast<int>(i);
  return -1;
}

int EnumType::member_index(const std::string& name) const {
  for (size_t i = 0; i < names.size(); ++i)
    if (names[i] == name) return static_cast<int>(i);
  return -1;
}

std::shared_ptr<Type> type_int() {
  if (!g_int) {
    g_int = std::make_shared<Type>();
    g_int->kind = TypeKind::Int64;
    g_int->name = "int";
  }
  return g_int;
}

std::shared_ptr<Type> type_float() {
  if (!g_float) {
    g_float = std::make_shared<Type>();
    g_float->kind = TypeKind::Double;
    g_float->name = "float";
  }
  return g_float;
}

std::shared_ptr<Type> type_bool() {
  if (!g_bool) {
    g_bool = std::make_shared<Type>();
    g_bool->kind = TypeKind::Bool;
    g_bool->name = "bool";
  }
  return g_bool;
}

std::shared_ptr<Type> type_string() {
  if (!g_string) {
    g_string = std::make_shared<Type>();
    g_string->kind = TypeKind::String;
    g_string->name = "string";
  }
  return g_string;
}

std::shared_ptr<Type> type_any() {
  if (!g_any) {
    g_any = std::make_shared<Type>();
    g_any->kind = TypeKind::Any;
    g_any->name = "Any";
  }
  return g_any;
}

std::shared_ptr<Type> type_type_info() {
  if (!g_type_info) {
    g_type_info = std::make_shared<Type>();
    g_type_info->kind = TypeKind::TypeInfo;
    g_type_info->name = "Type_Info";
  }
  return g_type_info;
}

std::shared_ptr<Type> type_type_info_tag() {
  if (!g_type_info_tag) {
    g_type_info_tag = std::make_shared<Type>();
    g_type_info_tag->kind = TypeKind::TypeInfoTag;
    g_type_info_tag->name = "Type_Info_Tag";
  }
  return g_type_info_tag;
}

std::shared_ptr<Type> type_type_table() {
  if (!g_type_table) {
    g_type_table = std::make_shared<Type>();
    g_type_table->kind = TypeKind::TypeTableView;
    g_type_table->name = "__type_table";
  }
  return g_type_table;
}

}  // namespace jai
