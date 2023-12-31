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

fn is_un_op(tkn: &Token) -> bool {
    match tkn {
        Token::TNeg | Token::TBitComp | Token::TLNeg => true,
        _ => false,
    }
}

/*
    Checks for Option<Token> == ">", "<", "<=", ">="
*/
fn is_rel_cmp(tkn: Option<&Token>) -> bool {
    match tkn {
        Some(inner) => inner == &Token::TLess || inner == &Token::TGreater 
        || inner == &Token::TLeq || inner == &Token::TGeq,
        _ => false
    }
}

/*
    Checks for Option<Token> == "!=" or "=="
*/
fn is_eq_cmp(tkn: Option<&Token>) -> bool {
    match tkn {
        Some(inner) => inner == &Token::TNeq || inner == &Token::TEq,
        _ => false
    }
}

/*
    Checks for Option<token> == "||"
*/
fn is_bool_or(tkn: Option<&Token>) -> bool {
    match tkn {
        Some(inner) => inner == &Token::TOr,
        _ => false
    }
}

/*
    Checks for Option<token> == "&&"
*/
fn is_bool_and(tkn: Option<&Token>) -> bool {
    match tkn {
        Some(inner) => inner == &Token::TAnd,
        _ => false
    }
}

/*
    Grammar:
    <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
*/
pub fn parse_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut log_and_exp = parse_log_and_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_log_and_exp;

    while is_bool_or(next) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            _ => panic!("parse_exp fail"),
        };
        next_log_and_exp = parse_log_and_exp(tokens);
        log_and_exp = ASTTree::BinaryOp(Box::new(log_and_exp), op, Box::new(next_log_and_exp));
        next = tokens.get(0);
    }
    return log_and_exp;
}

/*
    Grammar:
    <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
*/
pub fn parse_log_and_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut eq_exp = parse_eq_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_eq_exp;

    while is_bool_and(next) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            _ => panic!("parse_log_and_exp fail"),
        };
        next_eq_exp = parse_eq_exp(tokens);
        eq_exp = ASTTree::BinaryOp(Box::new(eq_exp), op, Box::new(next_eq_exp));
        next = tokens.get(0);
    }
    return eq_exp;
}

pub fn parse_eq_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut rel_exp = parse_add_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_rel_exp;

    while is_eq_cmp(next) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            _ => panic!("parse_eq_exp fail")
        };
        next_rel_exp = parse_rel_exp(tokens);
        rel_exp = ASTTree::BinaryOp(Box::new(rel_exp), op, Box::new(next_rel_exp));
        next = tokens.get(0);
    }
    return rel_exp;
}

/*
    Grammar:
    <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
*/
pub fn parse_rel_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut add_exp = parse_add_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_add_exp;

    while is_rel_cmp(next) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            _ => panic!("parse_rel_exp fail"),
        };
        next_add_exp = parse_add_exp(tokens);
        add_exp = ASTTree::BinaryOp(Box::new(add_exp), op, Box::new(next_add_exp));
        next = tokens.get(0);

    }
    return add_exp;
}

/*
    Grammar:
    <statement> ::= "return" <exp> ";"
*/
pub fn parse_statement(tokens: &mut VecDeque<Token>) -> ASTTree {
    if tokens.pop_front() != Some(Token::TReturn) {
        panic!("Parse exp return fail");
    }

    let exp = parse_add_exp(tokens);
    chk_semi(tokens);

    return ASTTree::Statement(Box::new(ASTTree::Return(Box::new(exp))));
}

/*
    Grammar:
    <exp> ::= <term> { ("+" | "-") <term> }
*/
pub fn parse_add_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut term = parse_term(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_term;

    while next == Some(&Token::TAdd) || next == Some(&Token::TNeg) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_add_exp")
        };
        next_term = parse_term(tokens);
        term = ASTTree::BinaryOp(Box::new(term), op, Box::new(next_term));
        next = tokens.get(0);
    }

    return term;
}

/*
    Grammar:
    <term> ::= <factor> { ("*" | "/") <factor> }
*/
pub fn parse_term(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut factor = parse_factor(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_factor;

    while next == Some(&Token::TMultiply) || next == Some(&Token::TDivide) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_term")
        };
        next_factor = parse_factor(tokens);
        factor = ASTTree::BinaryOp(Box::new(factor), op, Box::new(next_factor));
        next = tokens.get(0);
    }

    return factor;
}

/*
    Grammar:
    <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
*/
pub fn parse_factor(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.pop_front() {
        // <factor> ::= "(" <exp> ")"
        Some(Token::TOpenParen) => {
            let exp = parse_add_exp(tokens);
            if tokens.pop_front() != Some(Token::TCloseParen) {
                panic!("Parse close paren fail");
            }
            return exp;
        },
        // <factor> ::= <int>
        Some(Token::TIntLit(x)) => {
            return ASTTree::Constant(x);
        },
        Some(inner) => 
            // <factor> ::= <unary_op> <factor>
            if is_un_op(&inner) {
                let factor = parse_factor(tokens);
                return ASTTree::UnaryOp(inner, Box::new(factor));
            } else {
                panic!("Parse factor fail");
            },
        _ => panic!("Parse factor fail"),
    }
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