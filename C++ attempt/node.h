#include <iostream>
#include <vector>

class Rval;
class Stmt;
class Var_def;
class Func_def;
class Glob_var;
class Class_def;

typedef std::vector<Rval*> RvalList;
typedef std::vector<Stmt*> StmtList;
typedef std::vector<Var_def*> Var_defList;
typedef std::vector<Func_def*> Func_defList;
typedef std::vector<Glob_var*> Glob_varList;
typedef std::vector<Class_def*> Class_defList;

#ifndef TYPE_MISMATCH_EX
#define TYPE_MISMATCH_EX
class type_mismatch: public std::exception { };
#endif

class Node {
public:
    virtual ~Node() {}
};

class Type : public Node {
public:
    std::string class_id;
    int token;
    int arr_dim;
    Type(std::string id) : class_id(id), arr_dim(0), token(0) { }
    Type(int token) : class_id(""), arr_dim(0), token(token) { }

    bool operator==(const Type& other) const {
        if(arr_dim != other.arr_dim)    {
            return false;
        }
        else if(token != 0 && token == other.token) {
            return true;
        }
        else if(token != 0 || other.token != 0) {
            return false;
        }
        else if(! class_id.compare(other.class_id)) { //string ids are the same
            return true;
        }
        return false;
    }
    bool operator!=(const Type& other) const {
        return ! (*this == other);
    }
};

class Rval : public Node {
public:
    Type& type;
    Rval(Type& type) : type(type) { }
};

class Lval : public Rval {
public:
    Lval(Type& type) : Rval(type) { }
};

class Program : public Node {
public:
    Func_defList functions;
    Glob_varList global_vars;
    Class_defList classes;
    Program() : functions(*new Func_defList()),
        global_vars(*new Glob_varList()), classes(*new Class_defList()) { }
};

class Id : public Lval {
public:
    std::string id;
    Id(std::string id) : id(id), Lval(*new Type(0)) { /* TODO: check table */ }
};

class Glob_var : public Node {
public:
    Id& id;
    Rval& rval;
    Glob_var(Id& id, Rval& rval) : id(id), rval(rval) { }
};

class Class_def : public Node {
public:
    std::string id;
    Var_defList& class_vars;
    Func_defList& class_funcs;
    Class_def() : class_vars(*new Var_defList()),
    class_funcs(*new Func_defList()) { }
};

class Var_def : public Node {
public:
    Type& type;
    Id& id;
    Var_def(Type& type, Id& id) : type(type), id(id) { }
};

class Block : public Node {
public:
    StmtList& stmt_list;
    Block() : stmt_list(*new StmtList) { }
};

class Func_def : public Node {
public:
    Id& id;
    Var_defList& params_list;
    Block& block;
    Func_def(Id& id, Var_defList& params_list, Block& block) :
        id(id), params_list(params_list), block(block) { }
};

class Stmt : public Node {
};

class Return : public Stmt {
public:
    Rval& rval;
    Return(Rval& rval) : rval(rval) { }
};

class If : public Stmt {
public:
    Rval& condition;
    Block& if_block;
    Block& else_block;
    If(Rval& condition, Block& if_block, Block& else_block) :
        condition(condition), if_block(if_block), else_block(else_block) { }
};

class For : public Stmt {
public:
    Rval& init;
    Rval& condition;
    Rval& post;
    Block& block;
    For(Rval& init, Rval& condition, Rval& post, Block& block) :
        init(init), condition(condition), post(post), block(block) { }
};

class Access : public Lval {
public:
    Lval& lhs;
    Lval& rhs;
    Access(Lval& lhs, Lval& rhs) : lhs(lhs), rhs(rhs), Lval(*new Type(0)) { }
};

class Access_arr : public Lval {
public:
    Lval& lval;
    Rval& index;
    Access_arr(Lval& lval, Rval& index) : lval(lval), index(index),
        Lval(*new Type(0)) { }
};

class Assign : public Rval {
public:
    Lval& lval;
    Rval& rval;
    Assign(Lval& lval, Rval& rval, Type& type) :
        lval(lval), rval(rval), Rval(type) { }
};

class Int_lit : public Rval {
public:
    long long value;
    Int_lit(long long value, int token) :
        value(value), Rval(*new Type(token)) { }
};

class Doub_lit : public Rval {
public:
    double value;
    Doub_lit(double value, int token) : value(value), Rval(*new Type(token)) { }
};

class Char_lit : public Rval {
public:
    char value;
    Char_lit(char value, int token) : value(value), Rval(*new Type(token)) { }
};

class Bool_lit : public Rval {
public:
    bool value;
    Bool_lit(bool value, int token) : value(value), Rval(*new Type(token)) { }
};

class Str_lit : public Rval {
public:
    std::string value;
    Str_lit(std::string value, int token) :
        value(value), Rval(*new Type(token)) { }
};

class Access_lval : public Rval {
public:
    Lval& lval;
    Access_lval(Lval& lval) : lval(lval), Rval(*new Type(0)) { }
};

class Func_call : public Rval {
public:
    Lval& func;
    RvalList& actuals;
    Func_call(Lval& func, RvalList& actuals) :
        func(func), actuals(actuals), Rval(*new Type(0)) { }
};

class BinaryOperator : public Rval {
public:
    int op;
    Rval& lhs;
    Rval& rhs;
    BinaryOperator(Rval& lhs, int op, Rval& rhs) :
        lhs(lhs), rhs(rhs), op(op), Rval(lhs.type) {
            if(lhs.type != rhs.type)    {
                throw type_mismatch();
            }
            printf("Type lhs: %d, type rhs: %d\n", lhs.type.token, rhs.type.token);
        }
};

class UnaryOperator : public Rval {
public:
    int op;
    Rval& rval;
    UnaryOperator(int op, Rval& rval) :
        op(op), rval(rval), Rval(rval.type) { }
};