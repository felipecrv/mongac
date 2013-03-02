#include <string>
#include "exception.h"
#include "ast.h"

using namespace monga;
using namespace std;

const char* InvalidAssignExn::what() const noexcept {
    string s = "assignment type mismatch, '" + lvalue_type->typeExp() +
        "' is imcompatible with '" + rvalue_type->typeExp() + '\'';
    return s.c_str();
}
