use std::vec::Vec;
use std::iter::FromIterator;
use std::collections::VecDeque;

use super::types::Token;
use super::types::ASTTree;

use super::debug::print_ast;

/*
    Pops ";", raises fail if it doesn't exist or 
    there's mismatch
*/
pub fn chk_semi(tokens: &mut VecDeque<Token>) {
    if tokens.pop_front() != Some(Token::TSemicolon) {
        panic!("Parse semicolon fail");
    }
}

/*
    Grammar:
    <statement> ::= "return" <exp> ";"
*/
pub fn parse_statement(tokens: &mut VecDeque<Token>) -> ASTTree {
    if tokens.pop_front() != Some(Token::TReturn) {
        panic!("Parse exp return fail");
    }

    let exp = parse_exp(tokens);
    chk_semi(tokens);

    return ASTTree::Statement(Box::new(ASTTree::Return(Box::new(exp))));
}

/*
    Grammar:
    <exp> ::= <int> | <unary_op> <exp>
*/
pub fn parse_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.pop_front() {
        Some(Token::TIntLit(x)) => {
            return ASTTree::Constant(x);
        },
        Some(tkn) => {
            let exp = parse_exp(tokens);
            return ASTTree::UnaryOp(tkn, Box::new(exp));
        },
        _ => panic!("Parse exp int fail"),
    };
}

/*
    Grammar:
    <function> ::= "int" <id> "(" ")" "{" <statement> "}"
*/
pub fn parse_function(tokens: &mut VecDeque<Token>) -> ASTTree {
    let func_type;
    match tokens.pop_front() {
        Some(Token::TInt) => func_type = Token::TInt,
        _ => panic!("Parse function type fail"),
    }
    
    let func_id;
    match tokens.pop_front() {
        Some(Token::TIdentifier(tkn)) => func_id = tkn,
        _ => panic!("Parse function id fail"),
    }

    // Parens 
    let lparen = tokens.pop_front();
    let rparen = tokens.pop_front();

    if lparen != Some(Token::TOpenParen) || rparen != Some(Token::TCloseParen) {
        panic!("Parse function args fail");
    }

    // Open bracket
    if tokens.pop_front() != Some(Token::TOpenBrace) {
        panic!("Parse function (open) bracket fail");
    }

    // Parse expression, assuming we only return
    // for now
    let ret_node = parse_statement(tokens);
    let func_node = ASTTree::Function(func_id, func_type, 
        Box::new(ret_node));

    // Close bracket
    if tokens.pop_front() != Some(Token::TCloseBrace) {
        panic!("Parse function (close) bracket fail");
    }

    return func_node;
}

pub fn parse_program(tokens: &mut VecDeque<Token>) -> ASTTree {
    let func_node = Box::new(parse_function(tokens));
    return ASTTree::Program(func_node);
}

pub fn parser(tkn_stack: Vec<Token>) -> ASTTree {
    // For now, expecting only function declarations
    // and return statements
    let mut tokens: VecDeque<Token> = VecDeque::from_iter(tkn_stack);
    let tree = parse_program(&mut tokens);

    return tree;
}