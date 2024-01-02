use std::vec::Vec;
use std::iter::FromIterator;
use std::collections::VecDeque;

use super::types::Token;
use super::types::ASTTree;
use super::debug;

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

fn extract_id(token: Option<Token>) -> String {
    match token {
        Some(Token::TIdentifier(id)) => return id,
        _ => panic!("extract_id called on non identifier token"),
    }
}

/*
    Grammar:
    <statement> ::= "return" <exp> ";"
                    | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
                    | <exp> ";"
*/
pub fn parse_statement(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.get(0) {
        Some(Token::TReturn) => {
            let ret = process_return(tokens);
            return ASTTree::Statement(Box::new(ret));
        }
        
        Some(Token::TIf) => {
            let if_block = parse_if(tokens);
            return ASTTree::Statement(Box::new(if_block));

        }
        _ => {
            let exp = parse_exp(tokens);
            chk_semi(tokens);
            
            return ASTTree::Statement(Box::new(exp));
        },
    }
}

// "if" "(" <exp> ")" <statement> [ "else" <statement> ]
fn parse_if(tokens: &mut VecDeque<Token>) -> ASTTree {
    let if_tkn = tokens.pop_front();
    if if_tkn != Some(Token::TIf) {
        panic!("parse_if could not find if");
    };

    let lparens = tokens.pop_front();
    if lparens != Some(Token::TOpenParen) {
        panic!("parse_if didn't find open paren");
    };

    let condition = parse_exp(tokens);

    let rparens = tokens.pop_front();
    if rparens != Some(Token::TCloseParen) {
        panic!("parse_if didn't find close paren");
    };

    let if_statement = parse_statement(tokens);

    match tokens.get(0) {
        Some(Token::TElse) => {
            tokens.pop_front();
            let else_statement = parse_statement(tokens);
            return ASTTree::Conditional(Box::new(condition),
        Box::new(if_statement), Some(Box::new(else_statement)));
        },
        _ => {
            return ASTTree::Conditional(Box::new(condition), 
        Box::new(if_statement), None);
        },
    };
}

fn process_return(tokens: &mut VecDeque<Token>) -> ASTTree {
    tokens.pop_front();
    let exp = parse_exp(tokens);
    chk_semi(tokens);

    return ASTTree::Return(Box::new(exp));
}

/*
    Grammar:
    <declaration> ::= "int" <id> [ = <exp> ] ";"
*/
fn process_declare(tokens: &mut VecDeque<Token>) -> ASTTree {
    tokens.pop_front();
    match tokens.pop_front() {
        Some(Token::TIdentifier(id)) => {
            match tokens.pop_front() {
                Some(Token::TAssign) => {
                    let exp = parse_exp(tokens);
                    chk_semi(tokens);
                    return ASTTree::Declare(id, Some(Box::new(exp)));
                },
                Some(Token::TSemicolon) => {
                    return ASTTree::Declare(id, None);
                },
                _ => panic!("failed process_declare"),
            }
        },
        _ => panic!("Invalid declaration in parse_statement"),
    };
}

/*
    Grammar:
    <exp> ::= <id> "=" <exp> | <term> { ("+" | "-") <term> }
*/
pub fn parse_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    // Need to check this way rather than pattern match as identifier
    // could be part of a term
    match tokens.get(0) {
        // <exp> ::= <id> "=" <exp>
        Some(Token::TIdentifier(id)) => {
            if tokens.get(1) == Some(&Token::TAssign) {
                let id = extract_id(tokens.pop_front());
                tokens.pop_front();
                let exp = parse_exp(tokens);
                return ASTTree::Assign(id, Box::new(exp));
            }
            else {
                return parse_exp_inner(tokens);
            };
        },
        // <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
        _ => return parse_exp_inner(tokens),
    }
}

/*
    Grammar:
   <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
*/
fn parse_exp_inner(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut logical_and_exp = parse_logical_and(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_logical_and_exp;

    while next == Some(&Token::TOr) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_logical_and_exp = parse_logical_and(tokens);
        logical_and_exp = ASTTree::BinaryOp(Box::new(logical_and_exp), 
        op, Box::new(next_logical_and_exp));
        next = tokens.get(0);
    }

    return logical_and_exp;
}

/*
    Grammar:
   <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
*/
fn parse_logical_and(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut eq_exp = parse_eq_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_eq_exp;

    while next == Some(&Token::TAnd) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_eq_exp = parse_eq_exp(tokens);
        eq_exp = ASTTree::BinaryOp(Box::new(eq_exp), op, Box::new(next_eq_exp));
        next = tokens.get(0);
    }

    return eq_exp;
}

/*
    Grammar:
    <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
*/
fn parse_eq_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut rel_exp = parse_relational_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_rel_exp;

    while next == Some(&Token::TEq) || next == Some(&Token::TNeq) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_rel_exp = parse_relational_exp(tokens);
        rel_exp = ASTTree::BinaryOp(Box::new(rel_exp), op, 
            Box::new(next_rel_exp));
        next = tokens.get(0);
    }

    return rel_exp;
}

/*
    Grammar:
    <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
*/
fn parse_relational_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut additive_exp = parse_additive_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_additive_exp;

    while next == Some(&Token::TLess) || next == Some(&Token::TLeq)
    || next == Some(&Token::TGreater) || next == Some(&Token::TGeq) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_additive_exp = parse_additive_exp(tokens);
        additive_exp = ASTTree::BinaryOp(Box::new(additive_exp), op, 
            Box::new(next_additive_exp));
        next = tokens.get(0);
    }

    return additive_exp;
}

/*
    Grammar:
    <additive-exp> ::= <term> { ("+" | "-") <term> }
*/
fn parse_additive_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut term = parse_term(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_term;

    while next == Some(&Token::TAdd) || next == Some(&Token::TNeg) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_exp")
        };
        next_term = parse_term(tokens);
        term = ASTTree::BinaryOp(Box::new(term), op, 
            Box::new(next_term));
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
    <factor> ::= "(" <exp> ")" | <int> | <id> | <unary_op> <factor>
*/
pub fn parse_factor(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.pop_front() {
        // <factor> ::= "(" <exp> ")"
        Some(Token::TOpenParen) => {
            let exp = parse_exp(tokens);
            if tokens.pop_front() != Some(Token::TCloseParen) {
                panic!("Parse close paren fail");
            }
            return exp;
        },
        // <factor> ::= <int>
        Some(Token::TIntLit(x)) => {
            return ASTTree::Constant(x);
        },
        // <factor> ::= <id>
        Some(Token::TIdentifier(id)) => {
            return ASTTree::Var(id);
        },
        Some(inner) => 
            // <factor> ::= <unary_op> <factor>
            if is_un_op(&inner) {
                let factor = parse_factor(tokens);
                return ASTTree::UnaryOp(inner, Box::new(factor));
            } else {
                panic!("Parse factor fail 1");
            },
        _ => panic!("Parse factor fail 2"),
    }
}

/*
    Grammar:
    <block-item> ::= <statement> | <declaration>
*/
fn parse_block_item(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.get(0) {
        // <block-item> ::= <declaration>
        Some(Token::TInt) => {
            let decl = process_declare(tokens);
            return ASTTree::BlockItem(Box::new(decl));
        },
        // <block-item> ::= <statement>
        _ => {
            let statement = parse_statement(tokens);
            return ASTTree::BlockItem(Box::new(statement));
        }
    }
}

/*
    Grammar:
    <function> ::= "int" <id> "(" ")" "{" { <statement> } "}"
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

    let mut body : Vec<Box<ASTTree>> = Vec::new();

    let mut next = tokens.get(0);

    while next != Some(&Token::TCloseBrace) {
        body.push(Box::new(parse_block_item(tokens)));
        next = tokens.get(0);
    }

    // Pop off the TCloseBrace at the end
    tokens.pop_front();

    let func_node = ASTTree::Function(func_id, func_type, body);

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