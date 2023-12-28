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