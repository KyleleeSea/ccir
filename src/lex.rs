use std::env;
use std::fs;
use std::vec::Vec;
use super::types::Token;
use super::types::LexerFlag;

// Parses a file's text and converts characters to stack of appropriate tokens 
pub fn lexer() -> Vec<Token> {
    // Read command line file
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    
    let contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");

    let mut intlit_acc: Option<String> = None;
    let mut id_acc: Option<String> = None;
    let mut flag: LexerFlag = LexerFlag::NoFlag;

    let chars = contents.chars();
    let mut tkn_stack = Vec::new();

    // High level idea: Iterate over all the characters, tracking whether
    // we've been accumulating a multicharacter intLit or identifier
    // We push the intLit and identifiers lazily once we encounter a character
    // that's not an intLit of identifier.

    // Currently does not support comments
    for c in chars {
        // Flag handles compounds like &&, ||, >>, etc
        match flag {
            LexerFlag::NoFlag => (),
            LexerFlag::Amp => if c == '&' {
                tkn_stack.push(Token::TAnd);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                tkn_stack.push(Token::TBitAnd);
            },
            LexerFlag::Pipe => if c == '|' {
                tkn_stack.push(Token::TOr);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                tkn_stack.push(Token::TBitOr)
            },
            LexerFlag::Excl => if c == '=' {
                tkn_stack.push(Token::TNeq);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                tkn_stack.push(Token::TLNeg);
            },
            LexerFlag::Eq => if c == '=' {
                tkn_stack.push(Token::TEq);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                tkn_stack.push(Token::TAssign);
            },
            LexerFlag::Less => if c == '<' {
                tkn_stack.push(Token::TLShift);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                if c == '=' {
                    tkn_stack.push(Token::TLeq);
                    flag = LexerFlag::NoFlag;
                    continue;
                }
                else {
                    tkn_stack.push(Token::TLess);
                }
            },
            LexerFlag::Greater => if c == '>' {
                tkn_stack.push(Token::TRShift);
                flag = LexerFlag::NoFlag;
                continue;
            }
            else {
                if c == '=' {
                    tkn_stack.push(Token::TGeq);
                    flag = LexerFlag::NoFlag;
                    continue;
                }
                else {
                    tkn_stack.push(Token::TGreater);
                }
            },
        }

        flag = LexerFlag::NoFlag;

        // If intLit accumulated and we encounter non intLit character, push
        match intlit_acc {
            Some (ref _inner) => match c {
                '0' ..= '9' => (),
                _ => {
                    tkn_stack.push(intlit_to_token(intlit_acc));
                    intlit_acc = None;
                }
            },
            None => (),
        }

        // If identif accumulated and we encounter non identif character, push
        match id_acc {
            None => (),
            Some (ref _inner) => if !c.is_alphanumeric() && (c != '_')
                {
                    tkn_stack.push(identifier_to_token(id_acc));
                    id_acc = None;
                }
            }

        match c {
            '{' => tkn_stack.push(Token::TOpenBrace),
            '}' => tkn_stack.push(Token::TCloseBrace),
            '(' => tkn_stack.push(Token::TOpenParen),
            ')' => tkn_stack.push(Token::TCloseParen),
            ';' => tkn_stack.push(Token::TSemicolon),
            '-' => tkn_stack.push(Token::TNeg),
            '~' => tkn_stack.push(Token::TBitComp),
            '!' => flag = LexerFlag::Excl,
            '+' => tkn_stack.push(Token::TAdd),
            '*' => tkn_stack.push(Token::TMultiply),
            '/' => tkn_stack.push(Token::TDivide),
            '%' => tkn_stack.push(Token::TMod),
            '&' => flag = LexerFlag::Amp,
            '|' => flag = LexerFlag::Pipe,
            '=' => flag = LexerFlag::Eq,
            '<' => flag = LexerFlag::Less,
            '>' => flag = LexerFlag::Greater,
            '^' => tkn_stack.push(Token::TXor),
            ':' => tkn_stack.push(Token::TColon),
            '?' => tkn_stack.push(Token::TQuestion),
            ',' => tkn_stack.push(Token::TComma),
            // skip whitespace
            '\t' | '\n' | '\r' | ' ' => continue,
            '0' ..= '9' => 
                {
                    // Must do this match as an identifier could have an int in it
                    match id_acc {
                        None => {
                            match intlit_acc {
                                // begin accumulating an intlit
                                None => 
                                    intlit_acc = Some(c.to_string()),
                                // continue accumulating intlit
                                Some (inner) => 
                                    intlit_acc = Some(format!("{}{}",inner.clone(), 
                                        c.to_string())),
                            }
                        },
                        Some(inner) => id_acc = Some(format!("{}{}", inner.clone(), 
                        c.to_string())),
                    }
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

fn identifier_to_token(id: Option<String>) -> Token {
    match id {
        None => panic!("called identifierToToken on None, shouldn't happen"),
        Some (inner) => match inner.as_str()
            {
                "return" => Token::TReturn,
                "int" => Token::TInt,
                "if" => Token::TIf,
                "else" => Token::TElse,
                "for" => Token::TFor,
                "while" => Token::TWhile,
                "do" => Token::TDo,
                "break" => Token::TBreak,
                "continue" => Token::TContinue,
                _ => Token::TIdentifier(inner),
            }
    }
}

fn intlit_to_token(int_lit: Option<String>) -> Token {
    match int_lit {
        None => panic!("called intLitToToken on None, shouldn't happen"), 
        // Cast to int
        Some (inner) => Token::TIntLit(inner.parse::<i64>().unwrap()),
    }
}
