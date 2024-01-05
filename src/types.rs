use std::vec::Vec;
use std::option::Option;

#[derive(PartialEq)]
#[derive(Clone)]
pub enum Token {
    TOpenBrace,
    TCloseBrace,
    TOpenParen,
    TCloseParen,
    TSemicolon,
    TInt,
    TReturn,
    TIdentifier(String),
    TIntLit(i64),
    TNeg,
    TBitComp,
    TLNeg,
    TAdd,
    TMultiply,
    TDivide,
    TBitAnd,
    TBitOr,
    TXor,
    TLShift,
    TRShift,
    TAnd,
    TOr,
    TAssign,
    TEq,
    TNeq,
    TLess,
    TLeq,
    TGreater,
    TGeq,
    TMod,
    TIf,
    TElse,
    TColon,
    TQuestion,
    TFor,
    TWhile,
    TDo,
    TBreak,
    TContinue,
    TComma,
}

pub enum LexerFlag {
    NoFlag,
    Amp,
    Excl,
    Pipe,
    Eq,
    Less,
    Greater
}

pub enum ASTTree {
    Program(Vec<Box<ASTTree>>),
    // Function declarations
    // Function(function name, list of arg names, func body)
    Function(String, Vec<String>, Option<Box<ASTTree>>),
    // Function calls
    // FuncCall(function name, list of expressions for args)
    FuncCall(String, Vec<Box<ASTTree>>),
    Constant(i64),
    Return(Box<ASTTree>),
    Statement(Box<ASTTree>),
    UnaryOp(Token, Box<ASTTree>),
    BinaryOp(Box<ASTTree>, Token, Box<ASTTree>),
    Declare(String, Option<Box<ASTTree>>),
    Var(String),
    Assign(String, Box<ASTTree>),
    BlockItem(Box<ASTTree>),
    // Conditional(exp (condition), statement (if), statement option (else))
    Conditional(Box<ASTTree>, Box<ASTTree>, Option<Box<ASTTree>>),
    // Compound(block_item list)
    Compound(Vec<Box<ASTTree>>),
    // CondExp(exp, exp, exp) 
    // the three expressions are the condition, 'if' expression and 'else' expression, respectively
    CondExp(Box<ASTTree>, Box<ASTTree>, Box<ASTTree>),
    // For(exp option (initial expression), exp (controlling expression),
    // exp option (post-expression), statement (body))
    For(Option<Box<ASTTree>>, Box<ASTTree>, Option<Box<ASTTree>>, Box<ASTTree>),
    // ForDecl(declaration (initial declaration ), exp (controlling exp),
    // exp option (post-expression), statement (body))
    ForDecl(Box<ASTTree>, Box<ASTTree>, Option<Box<ASTTree>>, Box<ASTTree>),
    // While(expression (condition), statement (body))
    While(Box<ASTTree>, Box<ASTTree>),
    // Do(statement (body), expression (condition))
    Do(Box<ASTTree>, Box<ASTTree>), 
    Break,
    Continue,
    NullExp,
}

#[derive(Clone)]
pub enum VarType {
    Reg(String),
    Stk(i32),
}
