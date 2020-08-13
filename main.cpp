#include <iostream>
#include <cctype>
#include <cstdio>
#include <string>
#include <bits/unique_ptr.h>
#include <map>
#include <utility>
#include <vector>
#include <typeinfo>
#include <cassert>
#include <iomanip>
#include <bitset>

using namespace std;
map<int, string> code;
vector<int> e[10010];
int st[10010], fa[10010], top;
map<int, int> address;
map<string, int> variable;
int siz[10010];
map<string, int> val_begin;
map<string, vector<int> > arr;

int ccnt;
int clst[1010];

vector<unsigned int> riscv;
vector<unsigned int> vip[10010];

int cur_address = 100000;
enum Token
{
    tok_int = -1,

    tok_let = -2,
    tok_input = -3,
    tok_exit = -4,

    tok_goto = -5,
    tok_if = -6,
    tok_then = -7,
    tok_for = -8,
    tok_end = -9,


    //primary
    tok_identifier = -10,
    tok_number = -11,
    tok_line_end = -12
};

static string IdentifierStr, OpStr;
static int NumVal;

string nowstr;
int strpos;

char getstr()
{
    return nowstr[strpos++];
}

char LastChar = ' ';

static int get_tok()
{

    while (LastChar == ' ')
        LastChar = getstr();
    if (isalpha(LastChar))
    {
        IdentifierStr = LastChar;
        while (isalnum(LastChar = getstr()))
            IdentifierStr += LastChar;
        if (IdentifierStr == "LET")
            return tok_let;
        if (IdentifierStr == "INPUT")
            return tok_input;
        if (IdentifierStr == "EXIT")
            return tok_exit;
        if (IdentifierStr == "GOTO")
            return tok_goto;
        if (IdentifierStr == "IF")
            return tok_if;
        if (IdentifierStr == "THEN")
            return tok_then;
        if (IdentifierStr == "FOR")
            return tok_for;
        if (IdentifierStr == "INT")
            return tok_int;
        return tok_identifier;
    }
    if (isdigit(LastChar))
    {
        string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getstr();
        } while (isdigit(LastChar));
        NumVal = stoi(NumStr);
        return tok_number;
    }
    OpStr = " ";
    if (LastChar == '\n')
    {
        LastChar = ' ';
        return tok_line_end;
    }
    char ThisChar = LastChar;
    LastChar = getstr();
    if (ThisChar == '=' || ThisChar == '<' || ThisChar == '>' || ThisChar == '!' || ThisChar == '|' || ThisChar == '&'
        || ThisChar == '+' || ThisChar == '-' || ThisChar == '*' || ThisChar == '/')
    {
        OpStr = ThisChar;
        if (LastChar == '=' || LastChar == '&' || LastChar == '|')
        {
            OpStr += LastChar;
            LastChar = getstr();
        }
    }
    return ThisChar;
}

namespace
{
    class ExprAST
    {
    public:
        virtual ~ExprAST() = default;
    };

    class NumberExprAST : public ExprAST
    {
    public:
        int Val;

        NumberExprAST(int Val) : Val(Val)
        {}
    };

    class VariableExprAST : public ExprAST
    {
    public:
        string Name;
        vector<unique_ptr<ExprAST> > Array;

        VariableExprAST(const string &Name) : Name(Name)
        {
            Array.clear();
        }

        VariableExprAST(const string &Name, vector<unique_ptr<ExprAST> > Array) :
                Name(Name), Array(move(Array))
        {}

    };

    class UnaryExprAST : public ExprAST
    {
    public:
        char Op;
        unique_ptr<ExprAST> Operand;

        UnaryExprAST(char Op, unique_ptr<ExprAST> Operand) : Op(Op), Operand(move(Operand))
        {}
    };

    class BinaryExprAST : public ExprAST
    {
    public:
        string Op;
        unique_ptr<ExprAST> LHS, RHS;

        BinaryExprAST(const string &Op, unique_ptr<ExprAST> LHS, unique_ptr<ExprAST> RHS)
                : Op(Op), LHS(move(LHS)), RHS(move(RHS))
        {}
    };

    class InputAST : public ExprAST
    {
    public:
        vector<string> Name;
        vector<vector<unique_ptr<ExprAST> > > Array;

        InputAST(const vector<string> &Name) : Name(Name)
        {
            Array.clear();
        }

        InputAST(const vector<string> &Name, vector<vector<unique_ptr<ExprAST> > > Array) :
                Name(Name), Array(move(Array))
        {}
    };

    class LetAST : public ExprAST
    {

    public:
        string Name;
        unique_ptr<ExprAST> Operand;
        vector<unique_ptr<ExprAST> > Array;

        LetAST(const string &Name, unique_ptr<ExprAST> Operand) : Name(Name), Operand(move(Operand))
        {
            Array.clear();
        }

        LetAST(const string &Name, vector<unique_ptr<ExprAST> > Array) :
                Name(Name), Operand(nullptr), Array(move(Array))
        {}

        LetAST(const string &Name, unique_ptr<ExprAST> Operand, vector<unique_ptr<ExprAST> > Array) :
                Name(Name), Operand(move(Operand)), Array(move(Array))
        {}
    };

    class ExitAST : public ExprAST
    {
    public:
        unique_ptr<ExprAST> Operand;

        ExitAST(unique_ptr<ExprAST> Operand) : Operand(move(Operand))
        {}
    };

    class GotoAST : public ExprAST
    {
    public:
        int Val;

        GotoAST(int Val) : Val(Val)
        {}
    };

    class IfAST : public ExprAST
    {
    public:
        int Val;
        unique_ptr<ExprAST> Operand;

        IfAST(int Val, unique_ptr<ExprAST> Operand) : Val(Val), Operand(move(Operand))
        {}
    };

    class ForAST : public ExprAST
    {
    public:
        string Name;
        vector<unique_ptr<ExprAST> > Array;
        unique_ptr<ExprAST> expr, cmp;

        ForAST(const string &Name, unique_ptr<ExprAST> expr, unique_ptr<ExprAST> cmp)
                : Name(Name), expr(move(expr)), cmp(move(cmp))
        {
            Array.clear();
        }

        ForAST(const string &Name, vector<unique_ptr<ExprAST> > Array, unique_ptr<ExprAST> expr,
               unique_ptr<ExprAST> cmp)
                : Name(Name), Array(move(Array)), expr(move(expr)), cmp(move(cmp))
        {}
    };

    class EndAST : public ExprAST
    {
    public:
        EndAST()
        {}
    };
}

map<int, unique_ptr<ExprAST> > Stmt;

static int CurTok;

static int getNextToken()
{
    return CurTok = get_tok();
}

static map<string, int> BinOpPrecedence;

static int GetTokPrecedence()
{
    if (!isascii(CurTok) || CurTok == ';')
        return -1;
    int TokPrecedence = BinOpPrecedence[OpStr];
    if (TokPrecedence <= 0)
        return -1;
    return TokPrecedence;
}

unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "LogError: %s\n", Str);
    exit(-1);
    return nullptr;
}

static unique_ptr<ExprAST> ParseExpression();

static unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextToken();
    return move(Result);
}

static unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken();
    auto V = ParseExpression();

    if (!V)
        return nullptr;
    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken();
    return V;
}

static unique_ptr<ExprAST> ParseIdentifierExpr()
{
    string IdName = IdentifierStr;
    getNextToken();
    if (CurTok == '[')
    {
        vector<unique_ptr<ExprAST> > Array;
        while (CurTok == '[')
        {
            getNextToken();
            Array.push_back(ParseExpression());
            getNextToken();
        }
        return make_unique<VariableExprAST>(IdName, move(Array));
    }
    return make_unique<VariableExprAST>(IdName);
}

static unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
        default:
            return LogError("unknown token when expecting an expression");
        case tok_identifier:
            return ParseIdentifierExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
    }
}

static unique_ptr<ExprAST> ParseUnary()
{
    if (!isascii(CurTok) || CurTok == '(')
        return ParsePrimary();
    int Op = CurTok;
    getNextToken();
    if (auto Operand = ParseUnary())
        return make_unique<UnaryExprAST>(Op, move(Operand));
    return nullptr;
}

static unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrecedence, unique_ptr<ExprAST> LHS)
{
    while (true)
    {
        int TokPrecedence = GetTokPrecedence();
        if (TokPrecedence < ExprPrecedence)
            return LHS;
        string BinOp = OpStr;
        getNextToken();
        auto RHS = ParseUnary();
        if (!RHS)
            return nullptr;
        int NextPrecedence = GetTokPrecedence();
        if (TokPrecedence < NextPrecedence)
        {
            RHS = ParseBinOpRHS(TokPrecedence + 1, move(RHS));
            if (!RHS)
                return nullptr;
        }
        LHS = make_unique<BinaryExprAST>(BinOp, move(LHS), move(RHS));
    }
}

static unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParseUnary();
    if (!LHS)
        return nullptr;
    return ParseBinOpRHS(0, move(LHS));
}


static unique_ptr<ExprAST> PreInput()
{
    getNextToken();

    getNextToken();
    vector<string> tmp;
    vector<vector<unique_ptr<ExprAST> > > Array;
    while (CurTok != tok_line_end)
    {
        if (CurTok == tok_identifier)
            tmp.push_back(IdentifierStr);
        else
        {
            getNextToken();
            continue;
        }
        getNextToken();
        vector<unique_ptr<ExprAST> > now;
        if (CurTok == '[')
        {
            while (CurTok == '[')
            {
                getNextToken();
                now.push_back(ParseExpression());
                getNextToken();
            }
        }
        Array.push_back(move(now));
    }
    return make_unique<InputAST>(tmp, move(Array));
}

static unique_ptr<ExprAST> PreLet()
{
    string Name;
    getNextToken();
    getNextToken();
    if (CurTok == '(')
    {
        getNextToken();
        Name = IdentifierStr;
        getNextToken();
    } else
    {
        if (CurTok != tok_identifier)
            LogError("Forbidden Value");
        Name = IdentifierStr;
    }
    getNextToken();
    vector<unique_ptr<ExprAST> > Array;
    if (CurTok == '[')
    {
        while (CurTok == '[')
        {
            getNextToken();
            Array.push_back(ParseExpression());
            getNextToken();
        }
    }
    getNextToken();
    if (CurTok == tok_int)
    {
        getNextToken();
        vector<unique_ptr<ExprAST> > Array;
        while (CurTok == '[')
        {
            getNextToken();
            Array.push_back(ParseExpression());
            getNextToken();
        }
        return make_unique<LetAST>(Name, move(Array));
    }
    auto Operand = ParseExpression();
    return make_unique<LetAST>(Name, move(Operand), move(Array));
}

static unique_ptr<ExprAST> PreExit()
{
    getNextToken();
    getNextToken();
    if (CurTok == tok_line_end)
    {
        LogError("FAIL");
    }
    auto Operand = ParseExpression();
    return make_unique<ExitAST>(move(Operand));
}

static unique_ptr<ExprAST> PreGoto()
{
    getNextToken();
    getNextToken();
    return make_unique<GotoAST>(NumVal);
}

static unique_ptr<ExprAST> PreIf()
{
    getNextToken();
    getNextToken();
    auto Operand = ParseExpression();
    getNextToken();
    return make_unique<IfAST>(NumVal, move(Operand));
}

static unique_ptr<ExprAST> PreFor()
{
    getNextToken();
    getNextToken();
    string Name = IdentifierStr;
    getNextToken();
    vector<unique_ptr<ExprAST> > Array;
    if (CurTok == '[')
    {
        while (CurTok == '[')
        {
            getNextToken();
            Array.push_back(ParseExpression());
            getNextToken();
        }
    } else if (CurTok != '=')
        LogError("FOR STMT FAIL");
    getNextToken();
    auto expr = ParseExpression();
    getNextToken();
    auto cmp = ParseExpression();
    return make_unique<ForAST>(Name, move(Array), move(expr), move(cmp));
}

static unique_ptr<ExprAST> PreEnd()
{
    return make_unique<EndAST>();
}

void pre()
{
    int num;
    while (cin >> num)
    {
        if (num == -1)
            break;
        getline(cin, code[num]);
        cerr << num << ' ' << code[num] << endl;
        strpos = 0;
        nowstr = code[num] + "\n ";
        LastChar = ' ';
        if (code[num].find("REM") != string::npos)
            continue;
        e[st[top]].push_back(num);
        if (code[num].find("END") != string::npos)
        {
            if (!top)
                LogError("END FOR ERROR");
            top--;
        } else if (code[num].find("FOR") != string::npos)
            st[++top] = num;
        if (code[num].find("LET") != string::npos)
        {
            Stmt[num] = PreLet();
            continue;
        }
        if (code[num].find("INPUT") != string::npos)
        {
            Stmt[num] = PreInput();
            continue;
        }
        if (code[num].find("EXIT") != string::npos)
        {
            Stmt[num] = PreExit();
            continue;
        }
        if (code[num].find("GOTO") != string::npos)
        {
            Stmt[num] = PreGoto();
            continue;
        }
        if (code[num].find("IF") != string::npos)
        {
            Stmt[num] = PreIf();
            continue;
        }
        if (code[num].find("END") != string::npos)
        {
            Stmt[num] = PreEnd();
            continue;
        }
        if (code[num].find("FOR") != string::npos)
        {
            Stmt[num] = PreFor();
            continue;
        }
        LogError("FAIL");
    }
}

//x28 the address of temporary variable

//reg[x]=num
void reg_num(int x, int num)
{
    unsigned int tmp;
    tmp = (num >> 12 << 12) | (x << 7) | (0b0110111);
    riscv.push_back(tmp);
    num &= 4095;
    if (num & (1 << 11))
    {
        tmp = ((1 << 10) << 20) | (x << 15) | (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        tmp = ((1 << 10) << 20) | (x << 15) | (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        num ^= 1 << 11;
    }
    tmp = ((num & 4095) << 20) | (x << 15) | (x << 7) | (0b0010011);
    riscv.push_back(tmp);
}

void special_reg_num(int x, int num)
{
    if (num < 0)
    {
        unsigned int tmp;
        tmp = (0b0010011);
        riscv.push_back(tmp);
        riscv.push_back(tmp);
        riscv.push_back(tmp);
        tmp = (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        tmp = (num << 20) | (x << 15) | (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        return;
    }
    unsigned int tmp;
    tmp = (num >> 12 << 12) | (x << 7) | (0b0110111);
    riscv.push_back(tmp);
    num &= 4095;
    if (num & (1 << 11))
    {
        tmp = ((1 << 10) << 20) | (x << 15) | (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        tmp = ((1 << 10) << 20) | (x << 15) | (x << 7) | (0b0010011);
        riscv.push_back(tmp);
        num ^= 1 << 11;
    } else
    {
        tmp = 0b0010011;
        riscv.push_back(tmp);
        riscv.push_back(tmp);
    }
    tmp = ((num & 4095) << 20) | (x << 15) | (x << 7) | (0b0010011);
    riscv.push_back(tmp);
}

void jump(int x, int num)
{
    special_reg_num(x, num * 4);
    int tmp = (x << 15) | (0b010 << 12) | (0b1100111);
    riscv.push_back(tmp);
}

void risc_number(int add, int num)
{
    int tmp;
    reg_num(28, add);//x[28]=add
    tmp = (num << 20) | (5 << 7) | 0b0010011;//x[5]=num
    riscv.push_back(tmp);
    tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
    riscv.push_back(tmp);
}

void risc_variable(int add, string name)
{
    if (!variable.count(name))
    {
        puts("undefined variable");
        exit(-1);
    }
    int tmp;
    reg_num(28, variable[name]);//x[28]=var[name]
    tmp = (18 << 20) | (28 << 15) | (28 << 7) | (0b0110011);//x[28]+=x[18]
    riscv.push_back(tmp);
    tmp = (28 << 15) | (0b010 << 12) | (5 << 7) | (0b0000011);//x[5]=mem[x[28]]
    riscv.push_back(tmp);
    reg_num(28, add);//x[28]=add;
    tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
    riscv.push_back(tmp);
}

void risc_binary(int add, BinaryExprAST *now)
{
    int tmp;
    reg_num(28, address[(int) now->LHS.get()]);//x[28]=LHS
    tmp = (28 << 15) | (0b010 << 12) | (6 << 7) | (0b0000011);//x[6]=mem[x[28]]
    riscv.push_back(tmp);

    reg_num(28, address[(int) now->RHS.get()]);//x[28]=RHS
    tmp = (28 << 15) | (0b010 << 12) | (7 << 7) | (0b0000011);//x[7]=mem[x[28]]
    riscv.push_back(tmp);

    tmp = (7 << 20) | (6 << 15) | (5 << 7) | (0b0110011);//x[5]=x[6]?x[7]
    if (now->Op == "+")
        tmp |= (0b000 << 12), riscv.push_back(tmp);
    else if (now->Op == "-")
        tmp |= (0b0100000 << 25), riscv.push_back(tmp);
    else if (now->Op == "*")
        tmp |= (0b0000001 << 25), riscv.push_back(tmp);
    else if (now->Op == "/")
        tmp |= (0b0000001 << 25) | (0b100 << 12), riscv.push_back(tmp);
    else if (now->Op == "!=")
    {
        tmp = (7 << 20) | (6 << 15) | (0b010 << 12) | (30 << 7) | (0b0110011);//x[30]=x[6]<x[7]
        riscv.push_back(tmp);
        tmp = (6 << 20) | (7 << 15) | (0b010 << 12) | (31 << 7) | (0b0110011);//x[31]=x[7]<x[6]
        riscv.push_back(tmp);
        tmp = (31 << 20) | (30 << 15) | (0b110 << 12) | (5 << 7) | (0b0110011);//x[5]=x[30]|x[31]
        riscv.push_back(tmp);
    } else if (now->Op == "==")
    {
        tmp = (7 << 20) | (6 << 15) | (0b010 << 12) | (30 << 7) | (0b0110011);//x[30]=x[6]<x[7]
        riscv.push_back(tmp);
        tmp = (6 << 20) | (7 << 15) | (0b010 << 12) | (31 << 7) | (0b0110011);//x[31]=x[7]<x[6]
        riscv.push_back(tmp);
        tmp = (31 << 20) | (30 << 15) | (0b110 << 12) | (5 << 7) | (0b0110011);//x[5]=x[30]|x[31]
        riscv.push_back(tmp);
        tmp = (1 << 20) | (5 << 15) | (0b100 << 12) | (5 << 7) | (0b0010011);//x[5]=x[5]^1
        riscv.push_back(tmp);
    } else if (now->Op == "<")
    {
        tmp = (7 << 20) | (6 << 15) | (0b010 << 12) | (5 << 7) | (0b0110011);//x[5]=x[6]<x[7]
        riscv.push_back(tmp);
    } else if (now->Op == ">")
    {
        tmp = (6 << 20) | (7 << 15) | (0b010 << 12) | (5 << 7) | (0b0110011);//x[5]=x[7]<x[6]
        riscv.push_back(tmp);
    } else if (now->Op == ">=")
    {
        tmp = (7 << 20) | (6 << 15) | (0b010 << 12) | (5 << 7) | (0b0110011);//x[5]=x[6]<x[7]
        riscv.push_back(tmp);
        tmp = (1 << 20) | (5 << 15) | (0b100 << 12) | (5 << 7) | (0b0010011);//x[5]=x[5]^1
        riscv.push_back(tmp);
    } else if (now->Op == "<=")
    {
        tmp = (6 << 20) | (7 << 15) | (0b010 << 12) | (5 << 7) | (0b0110011);//x[5]=x[7]<x[6]
        riscv.push_back(tmp);
        tmp = (1 << 20) | (5 << 15) | (0b100 << 12) | (5 << 7) | (0b0010011);//x[5]=x[5]^1
        riscv.push_back(tmp);
    } else if (now->Op == "&&")
    {
        tmp = (7 << 20) | (6 << 15) | (0b111 << 12) | (5 << 7) | (0b0110011);//x[5]=x[6]&x[7]
        riscv.push_back(tmp);
    } else if (now->Op == "||")
    {
        tmp = (7 << 20) | (6 << 15) | (0b110 << 12) | (5 << 7) | (0b0110011);//x[5]=x[6]|x[7]
        riscv.push_back(tmp);
    }
    reg_num(28, add);//x[28]=add;
    tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
    riscv.push_back(tmp);
}

void risc_Unary(int add, UnaryExprAST *now)
{
    int tmp;
    reg_num(28, address[(int) now->Operand.get()]);//x[28]=OPERAND
    tmp = (28 << 15) | (0b010 << 12) | (7 << 7) | (0b0000011);//x[7]=mem[x[28]]
    riscv.push_back(tmp);

    tmp = (7 << 20) | (0 << 15) | (5 << 7) | (0b0110011);//x[5]=x[0]?x[7]
    assert(now->Op == '-');
    if (now->Op == '-')
    {
        tmp |= (0b0100000 << 25);

    }
    riscv.push_back(tmp);//x[5]=x[0]-x[7]
    reg_num(28, add);//x[28]=add;
    tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
    riscv.push_back(tmp);
}

void travel(ExprAST *now)
{
    string id = typeid(*now).name();
    if (id.find("NumberExprAST") != string::npos)
    {
        cur_address += 4;
        address[(int) now] = cur_address;
        risc_number(cur_address, ((NumberExprAST *) now)->Val);
        return;
    }
    if (id.find("VariableExprAST") != string::npos)
    {
        auto nt = (VariableExprAST *) now;
        reg_num(18, 0);
        if (!nt->Array.empty())
        {
            if (!arr.count(nt->Name))
                LogError("Undefined Array");
            for (int i = 0; i < (int) nt->Array.size(); i++)
            {
                reg_num(19, arr[nt->Name][i]);//x[19]=siz[i]
                int tmp;
                tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                riscv.push_back(tmp);
                travel(nt->Array[i].get());
                tmp = (5 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]+=x[5]
                riscv.push_back(tmp);
            }
        }
        reg_num(19, 4);
        int tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
        riscv.push_back(tmp);
        cur_address += 4;
        address[(int) now] = cur_address;
        risc_variable(cur_address, ((VariableExprAST *) now)->Name);
        return;
    }
    if (id.find("BinaryExprAST") != string::npos)
    {
        travel(((BinaryExprAST *) now)->LHS.get());
        travel(((BinaryExprAST *) now)->RHS.get());
        cur_address += 4;
        address[(int) now] = cur_address;
        risc_binary(cur_address, (BinaryExprAST *) now);
        return;
    }
    if (id.find("UnaryExprAST") != string::npos)
    {
        travel(((UnaryExprAST *) now)->Operand.get());
        cur_address += 4;
        address[(int) now] = cur_address;
        risc_Unary(cur_address, (UnaryExprAST *) now);
        return;
    }
}

//analysis the x th line
void analysis(int x)
{
    ExprAST *ptr = Stmt[x].get();
    string id = typeid(*ptr).name();
    if (id.find("LetAST") != string::npos)
    {
        riscv.clear();
        auto now = (LetAST *) ptr;
        if (now->Array.empty())
        {
            travel(now->Operand.get());
            if (!variable.count(now->Name))
            {
                cur_address += 4;
                variable[now->Name] = cur_address;
            }

            reg_num(28, variable[now->Name]);
            int tmp;
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
            siz[x] = riscv.size();
        } else if (now->Operand.get() == nullptr)
        {
            int arr_siz = 1;
            vector<int> tmp;
            for (int i = 0; i < (int) now->Array.size(); i++)
            {
                auto v = now->Array[i].get();
                string name = typeid(*v).name();
                if (name.find("VariableExprAST") != string::npos)
                {
                    tmp.push_back(val_begin[((VariableExprAST *) v)->Name]);
                    arr_siz *= tmp.back();
                } else if (name.find("NumberExprAST") != string::npos)
                {
                    tmp.push_back(((NumberExprAST *) v)->Val);
                    arr_siz *= tmp.back();
                } else
                {
                    assert(0);
                }
            }
            arr[now->Name] = tmp;
            cur_address += 4;
            variable[now->Name] = cur_address;
            cur_address += 4 * arr_siz;
        } else
        {
            reg_num(18, 0);
            for (int i = 0; i < (int) now->Array.size(); i++)
            {
                reg_num(19, arr[now->Name][i]);//x[19]=siz[i]
                int tmp;
                tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                riscv.push_back(tmp);
                string ttname = typeid(*(now->Array[i].get())).name();
                if (ttname.find("NumberExprAST") != string::npos)
                {
                    if (((NumberExprAST *) now->Array[i].get())->Val >= arr[now->Name][i])
                        LogError("Array Range Failed");
                }
                travel(now->Array[i].get());
                tmp = (5 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]+=x[5]
                riscv.push_back(tmp);
            }
            reg_num(19, 4);
            int tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
            riscv.push_back(tmp);
            tmp = (18 << 15) | (20 << 7) | (0b0110011);//x[20]=x[18]
            riscv.push_back(tmp);
            travel(now->Operand.get());
            if (!variable.count(now->Name))
            {
                cur_address += 4;
                variable[now->Name] = cur_address;
            }
            reg_num(28, variable[now->Name]);
            tmp = (20 << 20) | (28 << 15) | (28 << 7) | (0b0110011);//x[28]+=x[20]
            riscv.push_back(tmp);
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
            siz[x] = riscv.size();
        }
        return;
    }
    if (id.find("InputAST") != string::npos)
    {
        riscv.clear();
        auto now = (InputAST *) ptr;
        int cur_seq = -1;
        for (auto name:now->Name)
        {
            cur_seq++;
            if (!variable.count(name))
            {
                cur_address += 4;
                variable[name] = cur_address;
            }
            reg_num(18, 0);
            if (!now->Array[cur_seq].empty())
            {
                for (int i = 0; i < (int) now->Array[cur_seq].size(); i++)
                {
                    reg_num(19, arr[name][i]);//x[19]=siz[i]
                    int tmp;
                    tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                    riscv.push_back(tmp);
                    travel(now->Array[cur_seq][i].get());
                    tmp = (5 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]+=x[5]
                    riscv.push_back(tmp);
                }
                reg_num(19, 4);
                int tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                riscv.push_back(tmp);
            }
            int tmp;
            reg_num(28, variable[name]);//x[28]=var[name]
            tmp = (18 << 20) | (28 << 15) | (28 << 7) | (0b0110011);//x[28]+=x[18]
            riscv.push_back(tmp);
            int input_val;
            cin >> input_val;
            cerr<<input_val<<endl;
            clst[++ccnt] = input_val;
            if (now->Array[cur_seq].empty())
                val_begin[name] = input_val;
            special_reg_num(5, input_val);//x[5]=input
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
        }
        siz[x] = riscv.size();
        return;
    }
    if (id.find("ExitAST") != string::npos)
    {
        riscv.clear();
        auto now = (ExitAST *) ptr;
        travel(now->Operand.get());
        int tmp;
        tmp = (5 << 15) | (10 << 7) | 0b0110011;//x[10]=x[5]
        riscv.push_back(tmp);
        tmp = 0x0ff00513;//the end return
        riscv.push_back(tmp);
        siz[x] = riscv.size();
        return;
    }
    if (id.find("GotoAST") != string::npos)
    {
        siz[x] = 5;
        return;
    }
    if (id.find("IfAST") != string::npos)
    {
        riscv.clear();
        auto now = (IfAST *) ptr;
        travel(now->Operand.get());
        int tmp;
        tmp = (5 << 15) | (0b11000 << 7) | (0b1100011);//if(!x[5]) pc+=6*4
        riscv.push_back(tmp);
        siz[x] = riscv.size() + 5;
        return;
    }
    if (id.find("ForAST") != string::npos)
    {
        riscv.clear();
        auto now = (ForAST *) ptr;
        travel(now->cmp.get());
        int tmp;
        tmp = (5 << 15) | (0b001 << 12) | (0b11000 << 7) | (0b1100011);//if(x[5]) pc+=6*4 ->enter the for
        riscv.push_back(tmp);
        /*x[5]=false
         *jump out the for
         *use jalr
         *need 5 steps
         */
        siz[x] = riscv.size() + 5;
        for (auto y:e[x])
        {
            analysis(y);
        }
        // solve the for statement
        riscv.clear();
        travel(now->expr.get());
        reg_num(28, variable[now->Name]);
        tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
        riscv.push_back(tmp);
        siz[e[x].back()] += riscv.size();
        return;
    }
    if (id.find("EndAST") != string::npos)
    {
        siz[x] = 5;
        return;
    }
    cerr << "!!!" << endl;
}


int lst = 0;

void dfs(int x)
{
    st[x] = st[lst] + siz[lst];
    lst = x;
    for (auto y:e[x])
    {
        fa[y] = x;
        dfs(y);
        siz[x] += siz[y];
    }
}

void solve(int x)
{
    for (auto y:e[x])
    {
        analysis(y);
    }
}

void out(unsigned int x)
{
    for (int i = 1; i <= 4; i++)
    {
        cout << hex << setw(2) << setfill('0') << x % 256;
        cout << ' ';
        x /= 256;
    }
}

void output(int x)
{
    for (auto v:vip[x])
        out(v);
    for (auto y:e[x])
        output(y);
}

//trans the x th line
void trans(int x)
{
    ExprAST *ptr = Stmt[x].get();
    string id = typeid(*ptr).name();
    if (id.find("LetAST") != string::npos)
    {
        riscv.clear();
        auto now = (LetAST *) ptr;
        if (now->Array.empty())
        {
            travel(now->Operand.get());
            if (!variable.count(now->Name))
            {
                cur_address += 4;
                variable[now->Name] = cur_address;
            }

            reg_num(28, variable[now->Name]);
            int tmp;
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
        } else if (now->Operand.get() == nullptr)
        {
            int arr_siz = 1;
            vector<int> tmp;
            for (int i = 0; i < (int) now->Array.size(); i++)
            {
                auto v = now->Array[i].get();
                string name = typeid(*v).name();
                if (name.find("VariableExprAST") != string::npos)
                {
                    tmp.push_back(val_begin[((VariableExprAST *) v)->Name]);
                    arr_siz *= tmp.back();
                } else if (name.find("NumberExprAST") != string::npos)
                {
                    tmp.push_back(((NumberExprAST *) v)->Val);
                    arr_siz *= tmp.back();
                } else
                {
                    assert(0);
                }
            }
            arr[now->Name] = tmp;
            cur_address += 4;
            variable[now->Name] = cur_address;
            cur_address += 4 * arr_siz;
        } else
        {
            reg_num(18, 0);
            for (int i = 0; i < (int) now->Array.size(); i++)
            {
                reg_num(19, arr[now->Name][i]);//x[19]=siz[i]
                int tmp;
                tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                riscv.push_back(tmp);
                travel(now->Array[i].get());
                tmp = (5 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]+=x[5]
                riscv.push_back(tmp);
            }
            reg_num(19, 4);
            int tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
            riscv.push_back(tmp);
            tmp = (18 << 15) | (20 << 7) | (0b0110011);//x[20]=x[18]
            riscv.push_back(tmp);
            travel(now->Operand.get());
            if (!variable.count(now->Name))
            {
                cur_address += 4;
                variable[now->Name] = cur_address;
            }
            reg_num(28, variable[now->Name]);
            tmp = (20 << 20) | (28 << 15) | (28 << 7) | (0b0110011);//x[28]+=x[20]
            riscv.push_back(tmp);
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
        }
        vip[x] = riscv;
        return;
    }
    if (id.find("InputAST") != string::npos)
    {
        riscv.clear();
        auto now = (InputAST *) ptr;
        int cur_seq = -1;
        for (auto name:now->Name)
        {
            cur_seq++;
            if (!variable.count(name))
            {
                cur_address += 4;
                variable[name] = cur_address;
            }
            reg_num(18, 0);
            if (!now->Array[cur_seq].empty())
            {
                for (int i = 0; i < (int) now->Array[cur_seq].size(); i++)
                {
                    reg_num(19, arr[name][i]);//x[19]=siz[i]
                    int tmp;
                    tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                    riscv.push_back(tmp);
                    travel(now->Array[cur_seq][i].get());
                    tmp = (5 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]+=x[5]
                    riscv.push_back(tmp);
                }
                reg_num(19, 4);
                int tmp = (0b0000001 << 25) | (19 << 20) | (18 << 15) | (18 << 7) | (0b0110011);//x[18]*=x[19]
                riscv.push_back(tmp);
            }
            int tmp;
            reg_num(28, variable[name]);//x[28]=var[name]
            tmp = (18 << 20) | (28 << 15) | (28 << 7) | (0b0110011);//x[28]+=x[18]
            riscv.push_back(tmp);
            int input_val = clst[++ccnt];
            special_reg_num(5, input_val);//x[5]=input
            tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
            riscv.push_back(tmp);
        }
        vip[x] = riscv;
        return;
    }
    if (id.find("ExitAST") != string::npos)
    {
        riscv.clear();
        auto now = (ExitAST *) ptr;
        travel(now->Operand.get());
        int tmp;
        tmp = (5 << 15) | (10 << 7) | 0b0110011;//x[10]=x[5]
        riscv.push_back(tmp);
        tmp = 0x0ff00513;//the end return
        riscv.push_back(tmp);
        vip[x] = riscv;
        return;
    }
    if (id.find("GotoAST") != string::npos)
    {
        riscv.clear();
        int target = ((GotoAST *) ptr)->Val;
        string type = typeid(*(Stmt[target].get())).name();
        if (type.find("ForAST") != string::npos)
            target = e[target].back();
        jump(28, st[target]);
        vip[x] = riscv;
        return;
    }
    if (id.find("IfAST") != string::npos)
    {
        riscv.clear();
        auto now = (IfAST *) ptr;
        int target = now->Val;

        travel(now->Operand.get());
        int tmp;
        tmp = (5 << 15) | (0b11000 << 7) | (0b1100011);//if(!x[5]) pc+=6*4
        riscv.push_back(tmp);
        if (!Stmt.count(target))
        {
            tmp = (0b0110011);//x[0]+=x[0]
            riscv.push_back(tmp);
            riscv.push_back(tmp);
            riscv.push_back(tmp);
            tmp = (0 << 15) | (10 << 7) | 0b0110011;//x[10]=x[0]
            riscv.push_back(tmp);
            tmp = 0x0ff00513;//the end return
            riscv.push_back(tmp);
        } else
        {
            string type = typeid(*Stmt[target].get()).name();
            if (type.find("ForAST") != string::npos)
                target = e[target].back();
            jump(28, st[target]);
        }
        vip[x] = riscv;
        return;
    }
    if (id.find("ForAST") != string::npos)
    {
        riscv.clear();
        auto now = (ForAST *) ptr;
        travel(now->cmp.get());
        int tmp;
        tmp = (5 << 15) | (0b001 << 12) | (0b11000 << 7) | (0b1100011);//if(x[5]) pc+=6*4 ->enter the for
        riscv.push_back(tmp);
        /*x[5]=false
         *jump out the for
         *use jalr
         *need 5 steps
         */
        jump(28, st[x] + siz[x]);
        vip[x] = riscv;
        for (auto y:e[x])
        {
            trans(y);
        }
        // solve the for statement
        riscv.clear();
        travel(now->expr.get());
        reg_num(28, variable[now->Name]);
        tmp = (5 << 20) | (28 << 15) | (0b010 << 12) | (0b0100011);//mem[x[28]]=x[5]
        riscv.push_back(tmp);

        vector<unsigned int> temp = vip[e[x].back()];
        vip[e[x].back()] = riscv;
        vip[e[x].back()].insert(vip[e[x].back()].end(), temp.begin(), temp.end());
        return;
    }
    if (id.find("EndAST") != string::npos)
    {
        riscv.clear();
        jump(28, st[fa[x]]);
        vip[x] = riscv;
        return;
    }
    cerr << "!!!" << endl;
}

int main()
{
    //freopen("in.txt", "r", stdin);
    //freopen("D:/Barrin/Documents/C++/test/in.txt", "w", stdout);
    BinOpPrecedence["||"] = 5;
    BinOpPrecedence["&&"] = 5;

    BinOpPrecedence["<"] = 10;
    BinOpPrecedence["<="] = 10;
    BinOpPrecedence[">"] = 10;
    BinOpPrecedence[">="] = 10;
    BinOpPrecedence["=="] = 10;
    BinOpPrecedence["!="] = 10;

    BinOpPrecedence["+"] = 20;
    BinOpPrecedence["-"] = 20;

    BinOpPrecedence["*"] = 40;
    BinOpPrecedence["/"] = 40;

    pre();

    solve(0);
    dfs(0);

    ccnt = 0;
    cur_address = 100000;
    arr.clear();
    address.clear();
    variable.clear();
    for (auto x:e[0])
    {
        trans(x);
    }

    output(0);
    return 0;
}
