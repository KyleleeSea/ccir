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

// pub struct Tokenizer<'a> {
//     currInd: usize,
//     text: &'a str
// }

// impl<'a> Tokenizer<'a> {
//     fn new(src: &str) -> Tokenizer {
//         Tokenizer {
//             currInd: 0,
//             text: src,
//         }
//     }

//     fn incrementInd(&mut self, amt: usize) {
//         self.currInd += amt;
//     }
// }
