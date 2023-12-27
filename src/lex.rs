use std::env;
use std::fs;
use std::vec::Vec;
use super::types;

pub fn lexer() -> Vec<types::Token> {
    // Read command line file
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    
    let mut contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    
    // Skip all whitespace and newlines by stripping immediately
    contents.retain(|c| !c.is_whitespace());

    let mut intlit_acc: Option<String> = None;
    let mut id_acc: Option<String> = None;

    let chars = contents.chars();
    let mut tknStack = Vec::new();

    for c in chars {
        // If intLit accumulated and we encounter non intLit character, push
        match intlit_acc {
            Some (ref _inner) => match c {
                '0' ..= '9' => (),
                _ => {
                    tknStack.push(identifierToToken(intlit_acc));
                    intlit_acc = None;
                }
            },
            None => (),
        }

        // If identif accumulated and we encounter non identif character, push
        match id_acc {
            Some (ref _inner) => if !c.is_alphabetic() && !(c == '_')
                {
                    tknStack.push(identifierToToken(id_acc));
                    id_acc = None;
                }
            _ => (),
        }

        match c {
            '{' => tknStack.push(types::Token::TOpenBrace),
            '}' => tknStack.push(types::Token::TCloseBrace),
            '(' => tknStack.push(types::Token::TOpenParen),
            ')' => tknStack.push(types::Token::TCloseParen),
            ';' => tknStack.push(types::Token::TSemicolon),
            '0' ..= '9' => match intlit_acc {
                None => 
                    intlit_acc = Some(c.to_string()),
                Some (inner) => 
                    intlit_acc = Some(format!("{}{}",inner.clone(), c.to_string())),
            },
            _ => if c.is_alphabetic() || c == '_' {
                match id_acc {
                    None => 
                        id_acc = Some(c.to_string()),
                    Some (inner) => 
                        id_acc = Some(format!("{}{}", inner.clone(), c.to_string())),
                }
            }
            else {
                panic!("Unrecognized character");
            }
    }
}

return tknStack;
}

fn identifierToToken(id: Option<String>) -> types::Token {
    match id {
        None => panic!("called identifierToToken on None, shouldn't happen"),
        Some (inner) => match inner.as_str()
            {
                "return" => types::Token::TReturn,
                "int" => types::Token::TInt,
                _ => types::Token::TIdentifier(inner),
            }
        // Some ("return") => types::Token::TReturn,
        // Some ("int") => types::Token::TInt,
        // Some (inner) => types::Token::TIdentifier(inner)
    }
}

fn intLitToToken(intLit: Option<String>) -> types::Token {
    match intLit {
        None => panic!("called intLitToToken on None, shouldn't happen"), 
        // Cast to int
        Some (inner) => types::Token::TIntLit(inner.parse::<i64>().unwrap()),
    }
}