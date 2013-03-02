#include <string>
#include "exception.h"
#include "ast.h"

using namespace monga;
using namespace std;

const char* InvalidAssignExn::what() const noexcept {
    string s = "assignment type mismatch, \"" + lvalue_type->toHumanReadableStr() +
        "\" is imcompatible with \"" + rvalue_type->toHumanReadableStr() + "\"";
    return s.c_str();
}
