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
}