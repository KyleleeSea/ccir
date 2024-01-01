use std::vec::Vec;
use std::option::Option;

#[derive(PartialEq)]
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
    Program(Box<ASTTree>),
    Function(String, Token, Vec<Box<ASTTree>>),
    Constant(i64),
    Return(Box<ASTTree>),
    Statement(Box<ASTTree>),
    UnaryOp(Token, Box<ASTTree>),
    BinaryOp(Box<ASTTree>, Token, Box<ASTTree>),
    Declare(String, Option<Box<ASTTree>>),
    Var(String),
    // Not use ASTTree::Exp at all currently...
    Exp(Box<ASTTree>),
    Assign(String, Box<ASTTree>)
}
