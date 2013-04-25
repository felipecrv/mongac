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
            ts = "<INVALID TYPE>";
            break;
    }

    if (t.arr_dim > 0 && t.type_tok == CHAR) {
        return "String";
    }
    if (t.arr_dim > 0) {
        return "Array [0..1000] of " + ts;
    }

    return ts;
}

void PascalCodeGen::genPassedAsArgExp(
        const Exp* exp,
        const Type* formal_arg_type,
        std::ostream& os) {
    if (exp->cached_type->type_tok == IF) {
        switch (formal_arg_type->type_tok) {
            case INT:
                os << "(_monga_bool_to_int(";
                exp->generateCode(this, os);
                os << "))";
                return;
            case FLOAT:
                os << "(_monga_bool_to_float(";
                exp->generateCode(this, os);
                os << "))";
                return;
        }
    }
    exp->generateCode(this, os);
}

void PascalCodeGen::genAsBool(const Exp* exp, ostream& os) {
    if (exp->cached_type == nullptr) {
        exp->generateCode(this, os);
        return;
    }
    switch (exp->cached_type->type_tok) {
        case INT:
            os << "(_monga_int_to_bool(";
            exp->generateCode(this, os);
            os << "))";
            break;
        case CHAR:
            os << "(_monga_char_to_bool(";
            exp->generateCode(this, os);
            os << "))";
            break;
        case FLOAT:
            os << "(_monga_float_to_bool(";
            exp->generateCode(this, os);
            os << "))";
            break;
        default:
            exp->generateCode(this, os);
            break;
    }
}

void PascalCodeGen::gen(const VarDecl* var_decl, ostream& os) {
    auto ident_it = var_decl->idents->items.begin();
    os << "_" << var_decl->scope_id << "_" << **ident_it;
    for (ident_it++;
            ident_it != var_decl->idents->items.end();
            ident_it++) {
        os << ", _" << var_decl->scope_id << "_" << **ident_it;
    }
    os << " : " << toPascalTypeStr(*var_decl->type) << ";";
}

void PascalCodeGen::gen(const SumExp* exp, ostream& os) {
    genBinaryExp<SumExp>(exp, "+", os);
}

void PascalCodeGen::gen(const SubExp* exp, ostream& os) {
    genBinaryExp<SubExp>(exp, "-", os);
}

void PascalCodeGen::gen(const MultExp* exp, ostream& os) {
    genBinaryExp<MultExp>(exp, "*", os);
}

void PascalCodeGen::gen(const DivExp* exp, ostream& os) {
    if (exp->exp1->cached_type->isIntegral() &&
            exp->exp2->cached_type->isIntegral()) {
        genBinaryExp<DivExp>(exp, "Div", os);
    } else {
        genBinaryExp<DivExp>(exp, "/", os);
    }
}

void PascalCodeGen::gen(const EqExp* exp, ostream& os) {
    genBinaryExp<EqExp>(exp, "=", os);
}

void PascalCodeGen::gen(const GreaterEqExp* exp, ostream& os) {
    genBinaryExp<GreaterEqExp>(exp, ">=", os);
}

void PascalCodeGen::gen(const LowerEqExp* exp, ostream& os) {
    genBinaryExp<LowerEqExp>(exp, "<=", os);
}

void PascalCodeGen::gen(const LowerExp* exp, ostream& os) {
    genBinaryExp<LowerExp>(exp, "<", os);
}

void PascalCodeGen::gen(const GreaterExp* exp, ostream& os) {
    genBinaryExp<GreaterExp>(exp, ">", os);
}

void PascalCodeGen::gen(const AndExp* exp, ostream& os) {
    genBoolBinaryExp<AndExp>(exp, "<=", os);
}

void PascalCodeGen::gen(const OrExp* exp, ostream& os) {
    genBoolBinaryExp<OrExp>(exp, "<=", os);
}

void PascalCodeGen::gen(const Var* var, std::ostream& os) {
    var->ident_exp->generateCode(this, os);
    for (const auto& sub : var->arr_subscripts.items) {
        os << "[";
        sub->generateCode(this, os);
        os << "]";
    }
}

void PascalCodeGen::gen(const IdentExp* exp, std::ostream& os) {
    os << "_" << exp->scope_id << "_" << *exp->id;
}

void PascalCodeGen::gen(const IntLiteral* exp, std::ostream& os) {
    os << exp->val;
}

void PascalCodeGen::gen(const FloatLiteral* exp, std::ostream& os) {
    os << exp->val;
}

void PascalCodeGen::gen(const StringLiteral* exp, std::ostream& os) {
    os << '\'';
    for (uint32_t i = 0; i < exp->val->size(); i++) {
        char c = (*exp->val)[i];
        switch (c) {
        case '\n':
            os << "'#10'";
            break;
        case '\t':
            os << "'#9'";
            break;
        case '\r':
            os << "'#13'";
            break;
        case '\'':
            os << "''";
            break;
        default:
            os << c;
            break;
        }
    }
    os << '\'';
}

void PascalCodeGen::gen(const FuncCallExp* exp, std::ostream& os) {
    if (*exp->func_ident->id == "print") {
        os << "WriteLn";
    } else {
        os << *exp->func_ident->id;
    }

    os << "(";
    auto arg_exp_it = exp->arg_exps->items.begin();
    const FuncType* func_type = (const FuncType*) exp->cached_func_type.get();
    auto arg_type_vec_ptr = func_type->variations[0].first;
    auto arg_type_it = arg_type_vec_ptr->items.begin();

    if (arg_exp_it != exp->arg_exps->items.end()) {
        genPassedAsArgExp((*arg_exp_it).get(), (*arg_type_it).get(), os);
        for(arg_exp_it++, arg_type_it++;
                arg_exp_it != exp->arg_exps->items.end();
                arg_exp_it++, arg_type_it++) {
            os << ", ";
            (*arg_exp_it)->generateCode(this, os);
        }
    }

    os << ")";
}

void PascalCodeGen::gen(const NewExp* exp, std::ostream& os) {
}

void PascalCodeGen::gen(const MinusExp* minus_exp, ostream& os) {
    os << "(-";
    minus_exp->exp->generateCode(this, os);
    os << ")";
}

void PascalCodeGen::gen(const NotExp* not_exp, ostream& os) {
    os << "(Not ";
    genAsBool(not_exp->exp.get(), os);
    os << ")";
}

void PascalCodeGen::gen(const IfStmt* if_stmt, ostream& os) {
    os << "If ";
    genAsBool(if_stmt->cond_exp.get(), os);
    os << " Then" << endl;
    if (if_stmt->else_cmd == nullptr) {
        os << "Begin" << endl;
        if_stmt->then_cmd->generateCode(this, os);
        os << "End;" << endl;
    } else {
        os << "Begin" << endl;
        if_stmt->then_cmd->generateCode(this, os);
        os << "End" << endl;
        os << "else" << endl;
        os << "Begin" << endl;
        if_stmt->else_cmd->generateCode(this, os);
        os << "End;" << endl;
    }
}

void PascalCodeGen::gen(const BlockStmt* block_stmt, ostream& os) {
    os << "Begin" << endl;
    block_stmt->block->generateCode(this, os);
    os << "End;" << endl;
}

void PascalCodeGen::gen(const WhileStmt* while_stmt, ostream& os) {
    os << "While (";
    genAsBool(while_stmt->cond_exp.get(), os);
    os << ") Do\nBegin" << endl;
    while_stmt->block->generateCode(this, os);
    os << "End;" << endl;
}

void PascalCodeGen::gen(const AssignStmt* assign_stmt, ostream& os) {
    if (typeid(*assign_stmt->rvalue) == typeid(NewExp)) {
        return;
    }
    assign_stmt->var->generateCode(this, os);
    os << " := ";
    genPassedAsArgExp(assign_stmt->rvalue.get(), assign_stmt->var->cached_type.get(), os);
    //assign_stmt->rvalue->generateCode(this, os);
    os << ";" << endl;
}

void PascalCodeGen::gen(const ReturnStmt* return_stmt, ostream& os) {
    if (return_stmt->exp == nullptr) {
        os << "Exit;" << endl;
    } else {
        os << this->current_func_ident << " := ";
        return_stmt->exp->generateCode(this, os);
        os << ";" << endl << "Exit;" << endl;
    }
}

void PascalCodeGen::gen(const FuncCallStmt* func_call_stmt, ostream& os) {
    func_call_stmt->exp->generateCode(this, os);
    os << ";" << endl;
}

void PascalCodeGen::gen(const Command* com, ostream& os) {
    com->stmt->generateCode(this, os);
}

void PascalCodeGen::gen(const Block* blk, ostream& os) {
    for (auto& com : blk->commands->items) {
        com->generateCode(this, os);
    }
}

void PascalCodeGen::gen(const Arg* arg, ostream& os) {
    os << *arg->id << ": " << toPascalTypeStr(*arg->type);
}

void PascalCodeGen::gen(const FuncDecl* func_decl, ostream& os) {
    bool is_procedure = func_decl->ret_type->isVoid();
    this->current_func_ident = *func_decl->id;

    if (is_procedure) {
        os << "Procedure " << *func_decl->id << ";" << endl;
    } else {
        os << "Function " << *func_decl->id << "(";
        auto arg_it = func_decl->args->items.begin();
        if (arg_it != func_decl->args->items.end()) {
            os << "_1_";
            (*arg_it)->generateCode(this, os);
            for (arg_it++; arg_it != func_decl->args->items.end(); arg_it++) {
                os << "; _1_";
                (*arg_it)->generateCode(this, os);
            }
        }
        os << "): " << toPascalTypeStr(func_decl->ret_type.get()) << ";" << endl;
    }
    os << "Var" << std::endl;
    os << "_at_least_one_var: Char;" << endl;
    genBlockVariableDeclarations(func_decl->block.get(), os);
    os << "Begin" << endl;
    func_decl->block->generateCode(this, os);
    os << "End;\n" << endl;
}

void PascalCodeGen::gen(const VarDeclVec* var_decls, ostream& os) {
    for (const auto& var_decl : var_decls->items) {
        var_decl->generateCode(this, os);
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

    os << "Function _monga_int_to_bool(i: Integer): Boolean;" << endl;
    os << "Begin\n  _monga_int_to_bool := i <> 0;\nEnd;\n" << endl;
    os << "Function _monga_float_to_bool(f: Real): Boolean;" << endl;
    os << "Begin\n  _monga_float_to_bool := (f > 0.0) Or (f < 0.0);\nEnd;\n" << endl;
    os << "Function _monga_char_to_bool(c: String): Boolean;" << endl;
    os << "Begin\n  _monga_char_to_bool := Ord(c[0]) <> 0;\nEnd;\n" << endl;
    os << "Function _monga_bool_to_int(b: Boolean): Integer;" << endl;
    os << "Begin\n  If (b) Then _monga_bool_to_int := 1 Else _monga_bool_to_int := 0;\nEnd;\n" << endl;
    os << "Function _monga_bool_to_float(b: Boolean): Real;" << endl;
    os << "Begin\n  If (b) Then _monga_bool_to_float := 1 Else _monga_bool_to_float := 0;\nEnd;\n" << endl;

    for (const auto& decl : prog->items) {
        if (typeid(*decl) != typeid(VarDecl)) {
            auto func_decl = (const FuncDecl*) decl.get();
            gen(func_decl, os);
        }
    }

    os << "Begin" << endl;
    os << "main();" << endl;
    os << "End.";
}
