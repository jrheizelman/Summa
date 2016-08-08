#include <iostream>
#include <vector>

class Rval;
class Lval;
class Stmt;

typedef std::vector<Rval*> RvalList;
typedef std::vector<Stmt*> StmtList;

class Node {
public:
    virtual ~Node() {}
};

class Block : public Node {
public:
    StmtList stmt_list;
    Block() { }
};

class Stmt : public Node {
};

class Assign : public Stmt {
public:
    Lval& lval;
    Rval& rval;
    Assign(Lval& lval, Rval& rval) : lval(lval), rval(rval) { }
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
    std::string id;
    Rval& init;
    Rval& condition;
    Stmt& post;
    Block& block;
    For(std::string id, Rval& init, Rval& condition, Stmt& post, Block& block) :
        id(id), init(init), condition(condition), post(post), block(block) { }
};

class Rval : public Node {
};

class Lval : public Rval {
};

class Id : public Lval {
public:
    std::string id;
    Id(std::string id) : id(id) { /* TODO: check symbol table */ }
};

class Access : public Lval {
public:
    Lval& lhs;
    Lval& rhs;
    Access(Lval& lhs, Lval& rhs) : lhs(lhs), rhs(rhs) { }
};

class Access_arr : public Lval {
public:
    Lval& lval;
    Rval& index;
    Access_arr(Lval& lval, Rval& index) : lval(lval), index(index) { }
};

class Int_lit : public Rval {
public:
    long long value;
    Int_lit(long long value) : value(value) { }
};

class Doub_lit : public Rval {
public:
    double value;
    Doub_lit(double value) : value(value) { }
};

class Char_lit : public Rval {
public:
    char value;
    Char_lit(char value) : value(value) { }
};

class Bool_lit : public Rval {
public:
    bool value;
    Bool_lit(bool value) : value(value) { }
};

class Str_lit : public Rval {
public:
    std::string value;
    Str_lit(std::string value) : value(value) { }
};

class Access_lval : public Rval {
public:
    Lval& lval;
    Access_lval(Lval& lval) : lval(lval) { }
};

class Func_call : public Rval {
public:
    Lval& func;
    RvalList& actuals;
    Func_call(Lval& func, RvalList& actuals) : func(func), actuals(actuals) { }
};

class BinaryOperator : public Rval {
public:
    int op;
    Rval& lhs;
    Rval& rhs;
    BinaryOperator(Rval& lhs, int op, Rval& rhs) :
        lhs(lhs), rhs(rhs), op(op) { }
};

class UnaryOperator : public Rval {
public:
    int op;
    Rval& rval;
    UnaryOperator(int op, Rval& rval) :
        op(op), rval(rval) { }
};