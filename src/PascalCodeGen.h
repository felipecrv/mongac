#ifndef PASCAL_CODE_GEN_H_
#define PASCAL_CODE_GEN_H_

#include "ast.h"

namespace monga {

class PascalCodeGen : CodeGen {
private:
    string toPascalTypeStr(Type t) const;
    void gen(const monga::VarDecl*, std::ostream&);
    void gen(const monga::VarDeclVec*, std::ostream&);
    void gen(const monga::Exp*, std::ostream&);
    void gen(const monga::Stmt*, std::ostream&);
    void gen(const monga::Arg*, ostream&);
    void gen(const monga::ArgsVec*, ostream&);
    void gen(const monga::FuncDecl*, std::ostream&);
    void gen(const monga::Block*, std::ostream&);
    void gen(const monga::Command*, std::ostream&);

public:
    void gen(const monga::Prog*, std::ostream&);
};

};

#endif /* PASCAL_CODE_GEN_H_ */
