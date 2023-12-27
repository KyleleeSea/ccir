use std::env;
use std::fs;
use std::vec::Vec;
use super::types;

pub fn lexer() {
    // Read command line file
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    
    let mut contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    
    // Skip all whitespace and newlines by stripping immediately
    contents.retain(|c| !c.is_whitespace());

    let intlit_acc: Option<String> = None;
    let id_acc: Option<String> = None;

    let mut chars = contents.chars();
    let mut tknStack = Vec::new();

    // Case 1) intlit None, runningid None, singleton character
    // Then add that singleton character to the stack and continue

    // Case 2) intlit None, runningid None, run into intlit char
    // Start accumulating and continue

    // Case 3) intlit None, runningid None, run into id char
    // Start accumulating and continue

    // Case 4) intlit some, runningid none, run into intlit char, not end
    // keep accumulating

    // Case 5) intlit some, runningid none, run into intlit char, is end
    // add to stack

    // Case 6) intlit some, runningid none, run into non intlit char
    // add to stack



    for c in chars {
        // if intLit none --> do nothing
        // if intLit some and character is special --> push
        // if intLit some and character is int --> do nothing

        match intlit_acc {
            Some (inner) => match c {
                '0' ... '9' => (),
                _ => tknStack.push(identifierToToken(intlit_acc))
            },
            None => ()
        }

        match id_acc {
            Some (inner) => if c.is_alphabetic() || c == '_' => ()
                else tknStack.push(identifierToToken(id_acc))
            _ => ()
        }

        match c {
            '{' => tknStack.push(types::Token::TOpenBrace),
            '}' => tknStack.push(types::Token::TCloseBrace),
            '(' => tknStack.push(types::Token::TOpenParen),
            ')' => tknStack.push(types::Token::TCloseParen),
            ';' => tknStack.push(types::Token::TSemicolon),
            '0' ... '9' => ????,
            c @ '_' | c if c.is_alphabetic() => tokenize_ident(data)
            .chain_err(|| "Couldn't tokenize an identifier")?,
        other => bail!(ErrorKind::UnknownCharacter(other)),
        }
        println!("char is {}", c);
    }

    // let mut tknStack = Vec::new();

    // tokenize(contents, tknStack);

    // println!("With text:\n{contents}");
}

fn identifierToToken(id: Option<String>) -> types::Token {
    match id {
        None => panic!("called identifierToToken on None, shouldn't happen"),
        Some ("return") => types::Token::TReturn,
        Some ("int") => types::Token::TInt,
        Some (inner) => Types::Token::TIdentifier(inner)
    }
}

fn intLitToToken(intLit: Option<String>) -> types::Token {
    match intLit {
        None => panic!("called intLitToToken on None, shouldn't happen"), 
        // Cast to int
        Some (inner) => Types::Token::TIntLit(intLit.parse::<i64>().unwrap();)
    }
}

// fn tokenize(contents: String, tknStack: Vec<types::Token>) {
//     let mut chars = contents.chars();

//     for c in chars {
//         println!("next {}", chars.next().unwrap());
//     }
// }

// fn tokenize_single_token(contents: String, tkn_stack: Vec, prev: TknAcc) {

// }