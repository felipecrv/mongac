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
        os << " " << op << " ";
        exp->exp2->generateCode(this, os);
        os << ")";
    }

    template <class BoolBinaryExpClass>
    void genBoolBinaryExp(
            const BoolBinaryExpClass* exp,
            std::string op,
            std::ostream& os) {
        os << "(";
        genAsBool(exp->exp1.get(), os);
        os << " " << op << " ";
        genAsBool(exp->exp2.get(), os);
        os << ")";
    }

    void genAsBool(const monga::Exp* exp, std::ostream& os);

    void genAsType(const monga::Exp*, const monga::Type*, std::ostream& os);

    void genBlockVariableDeclarations(const BlockStmt* block_stmt, std::ostream& os) {
        genBlockVariableDeclarations(block_stmt->block.get(), os);
    }

    void genBlockVariableDeclarations(const Stmt* stmt, std::ostream& os) {
        if (typeid(*stmt) == typeid(IfStmt)) {
            auto if_stmt = (const IfStmt*) stmt;
            genBlockVariableDeclarations(if_stmt->then_cmd.get(), os);
            if (if_stmt->else_cmd != nullptr) {
                genBlockVariableDeclarations(if_stmt->else_cmd.get(), os);
            }
        } else if (typeid(*stmt) == typeid(BlockStmt)) {
            auto blk_stmt = (const BlockStmt*) stmt;
            genBlockVariableDeclarations(blk_stmt, os);
        } else if (typeid(*stmt) == typeid(WhileStmt)) {
            auto while_stmt = (const WhileStmt*) stmt;
            genBlockVariableDeclarations(while_stmt->block.get(), os);
        }
    }

    void genBlockVariableDeclarations(const Command* com, std::ostream& os) {
        genBlockVariableDeclarations(com->stmt.get(), os);
    }

    void genBlockVariableDeclarations(const Block* blk, std::ostream& os) {
        blk->vars->generateCode(this, os);
        for (unsigned int i = 0; i < blk->commands->items.size(); i++) {
            genBlockVariableDeclarations(blk->commands->items[i].get(), os);
        }
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
