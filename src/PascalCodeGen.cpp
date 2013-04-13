#include <iostream>
#include "ast.h"
#include "PascalCodeGen.h"
#include "tokens.h"
using namespace monga;
#include "parser.h"

using namespace std;

string PascalCodeGen::toPascalTypeStr(Type t) const {
    if (t.isFuncType() || t.isVoid()) {
        return "<INVALID TYPE>";
    }
    string ts;
    switch (t.type_tok) {
        case INT:
            ts = "Integer";
            break;
        case FLOAT:
            ts = "Real";
            break;
        case CHAR:
            ts = "Char";
            break;
        default:
            return "<INVALID TYPE>";
    }

    // TODO: implement array types
    return ts;
}

void PascalCodeGen::gen(const VarDecl* var_decl, ostream& os) {
    auto ident_it = var_decl->idents->items.begin();
    os << **ident_it;
    for (ident_it++;
            ident_it != var_decl->idents->items.end();
            ident_it++) {
        os << ", " << **ident_it;
    }
    os << " : " << toPascalTypeStr(*var_decl->type) << ";";
}

void PascalCodeGen::gen(const Exp* exp, ostream& os) {
    os << "<exp>";
}

void PascalCodeGen::gen(const Stmt* stmt, ostream& os) {
    if (typeid(*stmt) == typeid(IfStmt)) {
        auto if_stmt = (const IfStmt*) stmt;
        os << "If (";
        gen(if_stmt->cond_exp.get(), os);
        os << ") Then" << endl;
        if (if_stmt->else_cmd == nullptr) {
            os << "Begin" << endl;
            gen(if_stmt->then_cmd.get(), os);
            os << "End;" << endl;
        } else {
            os << "Begin" << endl;
            gen(if_stmt->then_cmd.get(), os);
            os << "End" << endl;
            os << "else" << endl;
            os << "Begin" << endl;
            gen(if_stmt->else_cmd.get(), os);
            os << "End;" << endl;
        }
    } else if (typeid(*stmt) == typeid(BlockStmt)) {
        auto block_stmt = (const BlockStmt*) stmt;
        os << "Begin" << endl;
        gen(block_stmt->block.get(), os);
        os << "End;" << endl;
    } else if (typeid(*stmt) == typeid(WhileStmt)) {
        auto while_stmt = (const WhileStmt*) stmt;
        os << "While (";
        gen(while_stmt->cond_exp.get(), os);
        os << ") {" << endl;
        gen(while_stmt->block.get(), os);
        os << "}" << endl;
    } else if (typeid(*stmt) == typeid(AssignStmt)) {
        auto assign_stmt = (const AssignStmt*) stmt;
        gen(assign_stmt->var.get(), os);
        os << " := ";
        gen(assign_stmt->rvalue.get(), os);
        os << ";" << endl;
    } else if (typeid(*stmt) == typeid(ReturnStmt)) {
        auto return_stmt = (const ReturnStmt*) stmt;
        if (return_stmt->exp == nullptr) {
            os << "Exit;" << endl;
        } else {
            os << "FUNC_NAME := ";
            gen(return_stmt->exp.get(), os);
            os << ";" << endl << "Exit;" << endl;
        }
    } else if (typeid(*stmt) == typeid(FuncCallStmt)) {
        auto func_call_stmt = (const FuncCallStmt*) stmt;
        gen(func_call_stmt->exp.get(), os);
    }
}

void PascalCodeGen::gen(const Command* com, ostream& os) {
    gen(com->stmt.get(), os);
}

void PascalCodeGen::gen(const Block* blk, ostream& os) {
    for (auto& com : blk->commands->items) {
        gen(com.get(), os);
    }
}

void PascalCodeGen::gen(const Arg* arg, ostream& os) {
    os << *arg->id << ": " << toPascalTypeStr(*arg->type);
}

void PascalCodeGen::gen(const ArgsVec* args, ostream& os) {
    os << "(";
    auto arg_it = args->items.begin();
    gen((*arg_it).get(), os);
    for (arg_it++; arg_it != args->items.end(); arg_it++) {
        os << ", ";
        gen((*arg_it).get(), os);
    }
    os << ")";
}

void PascalCodeGen::gen(const FuncDecl* func_decl, ostream& os) {
    bool is_procedure = func_decl->ret_type->isVoid();
    if (is_procedure) {
        os << "Procedure ";
        gen(func_decl->args.get(), os);
        os << endl;
    } else {
        os << "Function ";
        gen(func_decl->args.get(), os);
        os << ": " << toPascalTypeStr(func_decl->ret_type.get()) << ";" << endl;
    }
    os << "Begin" << endl;
    gen(func_decl->block.get(), os);
    os << "End;" << endl;
}

void PascalCodeGen::gen(const VarDeclVec* var_decls, ostream& os) {
    for (const auto& var_decl : var_decls->items) {
        gen(var_decl.get(), os);
        os << endl;
    }
}

void PascalCodeGen::gen(const Prog* prog, ostream& os) {
    os << "Program AProgram;" << endl;

    bool has_global_vars = false;
    for (const auto& decl : prog->items) {
        if (typeid(*decl) == typeid(VarDecl)) {
            if (!has_global_vars) {
                os << "Var" << endl;
                has_global_vars = true;
            }
            auto var_decl = (const VarDecl*) decl.get();
            gen(var_decl, os);
            os << endl;
        }
    }

    os << endl;

    for (const auto& decl : prog->items) {
        if (typeid(*decl) != typeid(VarDecl)) {
            auto func_decl = (const FuncDecl*) decl.get();
            gen(func_decl, os);
        }
    }
    os << endl;

    os << "Begin" << endl;
    os << "End.";
}
