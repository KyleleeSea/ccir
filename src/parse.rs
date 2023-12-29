use std::vec::Vec;
use std::iter::FromIterator;
use std::collections::VecDeque;

use super::types::Token;
use super::types::ASTTree;

use super::debug::print_ast;


/*
    Grammar:
    <exp> ::= "return" TIntLit(i64) TSemicolon
*/
pub fn parse_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    if tokens.pop_front() != Some(Token::TReturn) {
        panic!("Parse exp return fail");
    }

    let int_lit;
    match tokens.pop_front() {
        Some(Token::TIntLit(x)) => int_lit = x,
        _ => panic!("Parse exp int fail"),
    }

    if tokens.pop_front() != Some(Token::TSemicolon) {
        panic!("Parse exp semicolon fail");
    }
    let int_const = ASTTree::Constant(int_lit);
    let exp_tree = ASTTree::Return(Box::new(int_const));
    return exp_tree;
}

/*
    Grammar:
    <function> ::= "int" <id> "(" ")" "{" <exp> "}"
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
        panic!("Parse function bracket fail");
    }

    // Parse expression, assuming we only return
    // for now
    let ret_node = parse_exp(tokens);
    let func_node = ASTTree::Function(func_id, func_type, 
        Box::new(ret_node));

    // Close bracket
    if tokens.pop_front() != Some(Token::TCloseBrace) {
        panic!("Parse function bracket fail");
    }

    return func_node;
}

pub fn parser(tkn_stack: Vec<Token>) {
    // For now, expecting only function declarations
    // and return statements
    let mut tokens: VecDeque<Token> = VecDeque::from_iter(tkn_stack);
    let tree = parse_function(&mut tokens);
    print_ast(tree);
}