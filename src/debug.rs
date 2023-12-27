use super::types;

pub fn print_TknVec(tknStack: Vec<types::Token>) {
    for el in &tknStack {
        match el {
            types::Token::TOpenBrace => println!("open brace"),
            types::Token::TCloseBrace => println!("close brace"),
            types::Token::TOpenParen => println!("("),
            types::Token::TCloseParen => println!(")"),
            types::Token::TSemicolon => println!(";"),
            types::Token::TInt => println!("int"),
            types::Token::TReturn => println!("return"),
            types::Token::TIdentifier(innerStr) => println!("{}", innerStr),
            types::Token::TIntLit(innerNum) => println!("{}", innerNum),
        }
    }
}