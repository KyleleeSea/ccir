use super::types;

pub fn print_tkn_vec(tkn_stack: Vec<types::Token>) {
    for el in &tkn_stack {
        match el {
            types::Token::TOpenBrace => println!("open brace"),
            types::Token::TCloseBrace => println!("close brace"),
            types::Token::TOpenParen => println!("("),
            types::Token::TCloseParen => println!(")"),
            types::Token::TSemicolon => println!(";"),
            types::Token::TInt => println!("int"),
            types::Token::TReturn => println!("return"),
            types::Token::TIdentifier(inner_str) => println!("{}", inner_str),
            types::Token::TIntLit(inner_num) => println!("{}", inner_num),
        }
    }
}