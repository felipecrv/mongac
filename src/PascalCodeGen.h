#ifndef PASCAL_CODE_GEN_H_
#define PASCAL_CODE_GEN_H_

#include <iostream>
#include <string>
#include "ast.h"

namespace monga {

class PascalCodeGen : public CodeGen {
private:
    std::string current_func_ident;

    string toPascalTypeStr(Type t) const;

    template <class BinaryExpClass>
    void genBinaryExp(const BinaryExpClass* exp, std::string op, std::ostream& os) {
        os << "(";
        exp->exp1->generateCode(this, os);
        os << op;
        exp->exp2->generateCode(this, os);
        os << ")";
    }

public:

    void gen(const monga::VarDecl*, std::ostream&);
    void gen(const monga::VarDeclVec*, std::ostream&);

    void gen(const monga::SumExp*, std::ostream&);
    void gen(const monga::SubExp*, std::ostream&);
    void gen(const monga::MultExp*, std::ostream&);
    void gen(const monga::DivExp*, std::ostream&);
    void gen(const monga::EqExp*, std::ostream&);
    void gen(const monga::GreaterEqExp*, std::ostream&);
    void gen(const monga::LowerEqExp*, std::ostream&);
    void gen(const monga::LowerExp*, std::ostream&);
    void gen(const monga::GreaterExp*, std::ostream&);
    void gen(const monga::AndExp*, std::ostream&);
    void gen(const monga::OrExp*, std::ostream&);

    void gen(const monga::Var*, std::ostream&);
    void gen(const monga::IdentExp*, std::ostream&);
    void gen(const monga::IntLiteral*, std::ostream&);
    void gen(const monga::FloatLiteral*, std::ostream&);
    void gen(const monga::StringLiteral*, std::ostream&);
    void gen(const monga::FuncCallExp*, std::ostream&);
    void gen(const monga::NewExp*, std::ostream&);

    void gen(const monga::MinusExp*, std::ostream&);
    void gen(const monga::NotExp*, std::ostream&);

    void gen(const monga::IfStmt*, std::ostream&);
    void gen(const monga::BlockStmt*, std::ostream&);
    void gen(const monga::WhileStmt*, std::ostream&);
    void gen(const monga::AssignStmt*, std::ostream&);
    void gen(const monga::ReturnStmt*, std::ostream&);
    void gen(const monga::FuncCallStmt*, std::ostream&);

    void gen(const monga::Arg*, ostream&);
    void gen(const monga::FuncDecl*, std::ostream&);
    void gen(const monga::Block*, std::ostream&);
    void gen(const monga::Command*, std::ostream&);

    void gen(const monga::Prog*, std::ostream&);
};

};

#endif /* PASCAL_CODE_GEN_H_ */
