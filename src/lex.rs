use std::env;
use std::fs;
use std::vec::Vec;
use super::types;

// Parses a file's text and converts characters to stack of appropriate tokens 
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
    let mut tkn_stack = Vec::new();

    // High level idea: Iterate over all the characters, tracking whether
    // we've been accumulating a multicharacter intLit or identifier
    // We push the intLit and identifiers lazily once we encounter a character
    // that's not an intLit of identifier. We know we must end on a non intLit
    // or identifier character because the c program must end with }
    // if the above assumption doesn't hold we can do one last check at the
    // end of the for loop

    // Currently does not support comments
    for c in chars {
        // If intLit accumulated and we encounter non intLit character, push
        match intlit_acc {
            Some (ref _inner) => match c {
                '0' ..= '9' => (),
                _ => {
                    tkn_stack.push(intLit_to_token(intlit_acc));
                    intlit_acc = None;
                }
            },
            None => (),
        }

        // If identif accumulated and we encounter non identif character, push
        match id_acc {
            None => (),
            Some (ref inner) => match inner.as_str() {
                "int" => {
                    tkn_stack.push(identifier_to_token(id_acc));
                    id_acc = None;
                },
                _ => if !c.is_alphabetic() && !(c == '_')
                {
                    tkn_stack.push(identifier_to_token(id_acc));
                    id_acc = None;
                }
            }
        }

        match c {
            '{' => tkn_stack.push(types::Token::TOpenBrace),
            '}' => tkn_stack.push(types::Token::TCloseBrace),
            '(' => tkn_stack.push(types::Token::TOpenParen),
            ')' => tkn_stack.push(types::Token::TCloseParen),
            ';' => tkn_stack.push(types::Token::TSemicolon),
            '-' => tkn_stack.push(types::Token::TNeg),
            '~' => tkn_stack.push(types::Token::TBitComp),
            '!' => tkn_stack.push(types::Token::TLNeg),
            '+' => tkn_stack.push(types::Token::TAdd),
            '*' => tkn_stack.push(types::Token::TMultiply),
            '/' => tkn_stack.push(types::Token::TDivide),
            '0' ..= '9' => match intlit_acc {
                // begin accumulating an intlit
                None => 
                    intlit_acc = Some(c.to_string()),
                // continue accumulating intlit
                Some (inner) => 
                    intlit_acc = Some(format!("{}{}",inner.clone(), 
                        c.to_string())),
            },
            _ => if c.is_alphabetic() || c == '_' {
                match id_acc {
                    None => 
                        id_acc = Some(c.to_string()),
                    Some (inner) => 
                        id_acc = Some(format!("{}{}", inner.clone(), 
                            c.to_string())),
                }
            }
            else {
                panic!("Unrecognized character");
            }
    }
}   

return tkn_stack;
}

fn identifier_to_token(id: Option<String>) -> types::Token {
    match id {
        None => panic!("called identifierToToken on None, shouldn't happen"),
        Some (inner) => match inner.as_str()
            {
                "return" => types::Token::TReturn,
                "int" => types::Token::TInt,
                _ => types::Token::TIdentifier(inner),
            }
    }
}

fn intLit_to_token(int_lit: Option<String>) -> types::Token {
    match int_lit {
        None => panic!("called intLitToToken on None, shouldn't happen"), 
        // Cast to int
        Some (inner) => types::Token::TIntLit(inner.parse::<i64>().unwrap()),
    }
}