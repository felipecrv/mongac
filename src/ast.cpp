#include <memory>
#include <vector>
#include <string>
#include <cstdio>
#include "ast.h"
using namespace monga;
#include "parser.h"

namespace monga {
using namespace std;

string toStr(const MongaAstNode& node) {
    return node.toStr();
}

string toStr(const string& s) {
    return s;
}

string MongaType::toStr() const {
    string type;
    switch (type_tok) {
        case INT:
            type = "int";
            break;
        case FLOAT:
            type = "char";
            break;
        case CHAR:
            type = "char";
            break;
        case VOID:
            type = "void";
            break;
        default:
            type = "<TYPE>";
            break;
    }

    char dim[20];
    sprintf(dim, "%d", array_dimensions);
    return "(t " + type + " " + string(dim) + ")";
}

}; // namespace monga
