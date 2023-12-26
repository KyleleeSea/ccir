pub enum Token {
    TOpenBrace,
    TCloseBrace,
    TOpenParen,
    TCloseParen,
    TSemicolon,
    TInt,
    TReturn,
    TIdentifier(String),
    TIntLit(i64)
}

pub enum TknAcc {
    TANone,
    TAIdentifier(String),
    TAIntLit(String)
}