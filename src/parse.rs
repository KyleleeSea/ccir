use std::vec::Vec;
use std::iter::FromIterator;
use std::collections::VecDeque;

use super::types::Token;
use super::types::ASTTree;
use super::debug;

/*
    Pops the top token from the stack, raises fail if it doesn't match
    target, emitting the err string
*/
fn chk_token(tokens: &mut VecDeque<Token>, target: Token, err: &str) {
    if tokens.pop_front() != Some(target) {
        panic!("could not find target {}", err);
    }
}

fn is_un_op(tkn: &Token) -> bool {
    match tkn {
        Token::TNeg | Token::TBitComp | Token::TLNeg => true,
        _ => false,
    }
}

fn is_bin_term_op(tkn: Option<&Token>) -> bool {
    match tkn {
        Some(inner) => *inner == Token::TMultiply || *inner == Token::TDivide || *inner == Token::TMod,
        _ => false,
    }
}

fn extract_id(token: Option<Token>) -> String {
    match token {
        Some(Token::TIdentifier(id)) => return id,
        _ => panic!("extract_id called on non identifier token"),
    }
}

fn is_for_decl(tokens: &mut VecDeque<Token>) -> bool {
    // Assumed at this point tokens.get(0) is "for", tokens.get(1) is "("
    match tokens.get(2) {
        Some(Token::TInt) => return true,
        _ => return false,
    };
}

/*
    Grammar:
    <statement> ::= "return" <exp> ";"
                    | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
                    | "{" { <block-item> } "}
                    | <null-exp> ";"
                    | "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
                    |"for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
                    | "while" "(" <exp> ")" <statement>
                    | "do" <statement> "while" "(" <exp> ")" ";"
                    | "break" ";"
                    | "continue" ";"
                    | <exp> ";"
*/
pub fn parse_statement(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.get(0) {
        // "return" <exp> ";"
        Some(Token::TReturn) => {
            let ret = parse_return(tokens);
            return ASTTree::Statement(Box::new(ret));
        }
        
        // "if" "(" <exp> ")" <statement> [ "else" <statement> ]
        Some(Token::TIf) => {
            let if_block = parse_if(tokens);
            return ASTTree::Statement(Box::new(if_block));

        },

        // "{" { <block-item> } "}
        Some(Token::TOpenBrace) => {
            let compound = parse_compound(tokens);
            return ASTTree::Statement(Box::new(compound));
        },

        // <null-exp> ";"
        Some(Token::TSemicolon) => {
            tokens.pop_front();
            return ASTTree::Statement(Box::new(ASTTree::NullExp));
        },

        Some(Token::TFor) => {
            let node;
            // "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
            if is_for_decl(tokens) {
                node = parse_for_decl(tokens);
            }
            // for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
            else {
                node = parse_for(tokens);
            }

            return ASTTree::Statement(Box::new(node));
        },

        // "while" "(" <exp> ")" <statement>
        Some(Token::TWhile) => {
            let while_node = parse_while(tokens);
            return ASTTree::Statement(Box::new(while_node));
        },

        // "do" <statement> "while" "(" <exp> ")" ";"
        Some(Token::TDo) => {
            let do_node = parse_do(tokens);
            return ASTTree::Statement(Box::new(do_node));
        },

        //  "break" ";"
        Some(Token::TBreak) => {
            tokens.pop_front();
            chk_token(tokens, Token::TSemicolon, "parse_statement, semicolon");
            return ASTTree::Statement(Box::new(ASTTree::Break));
        },

        // "continue" ";"
        Some(Token::TContinue) => {
            tokens.pop_front();
            chk_token(tokens, Token::TSemicolon, "parse_statement, semicolon");
            return ASTTree::Statement(Box::new(ASTTree::Continue));
        },

        // <exp> ";"
        _ => {
            let exp = parse_exp(tokens);
            chk_token(tokens, Token::TSemicolon, "parse_statement, semicolon");
            
            return ASTTree::Statement(Box::new(exp));
        },
    }
}

/*
    Grammar:
    "do" <statement> "while" "(" <exp> ")" ";"
*/
fn parse_do(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TDo, "parse_do, do");

    let body = parse_statement(tokens);

    chk_token(tokens, Token::TWhile, "parse_do, while");
    chk_token(tokens, Token::TOpenParen, "parse_do, open paren");

    let condition = parse_exp(tokens);

    chk_token(tokens, Token::TCloseParen, "parse_do, close paren");
    chk_token(tokens, Token::TSemicolon, "parse_do, semicolon");  

    return ASTTree::Do(Box::new(body), Box::new(condition));
}

/*
    Grammar:
    "while" "(" <exp> ")" <statement>
*/
fn parse_while(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TWhile, "parse_while, while");
    chk_token(tokens, Token::TOpenParen, "parse_while, open paren");

    let condition = parse_exp(tokens);
    chk_token(tokens, Token::TCloseParen, "parse_while, close paren");
    let body = parse_statement(tokens);

    return ASTTree::While(Box::new(condition), Box::new(body));
}

/*
    Grammar:
    "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
*/
fn parse_for_decl(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TFor, "parse_for_decl, for");
    chk_token(tokens, Token::TOpenParen, "parse_for_decl, open paren");

    let controlling;
    let postop;

    let decl = Box::new(parse_declare(tokens));

    match tokens.get(0) {
        Some(Token::TSemicolon) => {
            tokens.pop_front();
            // not sure if this is correct / complies with code gen format
            // may need to wrap the constant in something
            controlling = Box::new(ASTTree::Constant(1));
        },
        _ => {
            controlling = Box::new(parse_exp(tokens));
            chk_token(tokens, Token::TSemicolon, "parse_for, semicolon");
        },
    };

    match tokens.get(0) {
        Some(Token::TCloseParen) => {
            tokens.pop_front();
            postop = None;
        },
        _ => {
            postop = Some(Box::new(parse_exp(tokens)));
            chk_token(tokens, Token::TCloseParen, "parse_for_decl, close paren");
        },
    };

    let body = Box::new(parse_statement(tokens));

    return ASTTree::ForDecl(decl, controlling, postop, body);
}

/*
    Grammar:
    for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
*/
fn parse_for(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TFor, "parse_for, for");
    chk_token(tokens, Token::TOpenParen, "parse_for, open paren");

    let controlling;
    let postop;
    let init;

    // <exp-option> ";"
    match tokens.get(0) {
        Some(Token::TSemicolon) => {
            tokens.pop_front();
            init = None;
        },
        _ => {
            init = Some(Box::new(parse_exp(tokens)));
            chk_token(tokens, Token::TSemicolon, "parse_for, semicolon");
        },
    };

    // <exp-option> ";"
    match tokens.get(0) {
        Some(Token::TSemicolon) => {
            tokens.pop_front();
            // not sure if this is correct / complies with code gen format
            // may need to wrap the constant in something
            controlling = Box::new(ASTTree::Constant(1));
        },
        _ => {
            controlling = Box::new(parse_exp(tokens));
            chk_token(tokens, Token::TSemicolon, "parse_for, semicolon");
        },
    };

    // <exp-option> ")"
    match tokens.get(0) {
        Some(Token::TCloseParen) => {
            tokens.pop_front();
            // not sure if this is correct / complies with code gen format
            // may need to wrap the constant in something
            postop = None;
        },
        _ => {
            postop = Some(Box::new(parse_exp(tokens)));
            chk_token(tokens, Token::TCloseParen, "parse_for, close paren");
        },
    };

    let body = Box::new(parse_statement(tokens));

    return ASTTree::For(init, controlling, postop, body);
}


/*
    Grammar:
    "{" { <block-item> } "}
*/
fn parse_compound(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TOpenBrace, "parse_compound, open brace");

    let mut block_item_list = Vec::new();

    let mut next = tokens.get(0);

    while next != Some(&Token::TCloseBrace) {
        block_item_list.push(Box::new(parse_block_item(tokens)));
        next = tokens.get(0);
    }

    // Remove close brace
    tokens.pop_front();

    return ASTTree::Compound(block_item_list);
}

// "if" "(" <exp> ")" <statement> [ "else" <statement> ]
fn parse_if(tokens: &mut VecDeque<Token>) -> ASTTree {
    chk_token(tokens, Token::TIf, "parse_if, if");
    chk_token(tokens, Token::TOpenParen, "parse_if, open paren");

    let condition = parse_exp(tokens);

    chk_token(tokens, Token::TCloseParen, "parse_if, close paren");

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

fn parse_return(tokens: &mut VecDeque<Token>) -> ASTTree {
    tokens.pop_front();
    let exp = parse_exp(tokens);
    chk_token(tokens, Token::TSemicolon, "parse_return, semicolon");

    return ASTTree::Return(Box::new(exp));
}

/*
    Grammar:
    <declaration> ::= "int" <id> [ = <exp> ] ";"
*/
fn parse_declare(tokens: &mut VecDeque<Token>) -> ASTTree {
    tokens.pop_front();
    match tokens.pop_front() {
        Some(Token::TIdentifier(id)) => {
            match tokens.pop_front() {
                Some(Token::TAssign) => {
                    let exp = parse_exp(tokens);
                    chk_token(tokens, Token::TSemicolon, "parse_declare, 
                        semicolon");
                    return ASTTree::Declare(id, Some(Box::new(exp)));
                },
                Some(Token::TSemicolon) => {
                    return ASTTree::Declare(id, None);
                },
                _ => panic!("failed parse_declare"),
            }
        },
        _ => panic!("Invalid declaration in parse_statement"),
    };
}

/*
    Grammar:
    <exp> ::= <id> "=" <exp> | <conditional-exp>
*/
pub fn parse_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    // Need to check this way rather than pattern match as identifier
    // could be part of a term
    match (tokens.get(0), tokens.get(1)) {
        // <exp> ::= <id> "=" <exp>
        (Some(Token::TIdentifier(_func_id)),
         Some(Token::TOpenParen)) => {
            return parse_conditional_exp(tokens);
        }
        (Some(Token::TIdentifier(_id)), _) => {
            if tokens.get(1) == Some(&Token::TAssign) {
                let id = extract_id(tokens.pop_front());
                tokens.pop_front();
                let exp = parse_exp(tokens);
                return ASTTree::Assign(id, Box::new(exp));
            }
            else {
                return parse_conditional_exp(tokens);
            };
        },
        // <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
        (_, _) => return parse_conditional_exp(tokens),
    }
}

/*
    Grammar:
    <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
*/
fn parse_conditional_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let logical_or_exp = parse_logical_or(tokens);
    let next_exp;
    let next_conditional_exp;

    match tokens.get(0) {
        Some(Token::TQuestion) => {
            // Parse
            tokens.pop_front();
            next_exp = parse_exp(tokens);

            match tokens.get(0) {
                Some(Token::TColon) => {
                    tokens.pop_front();
                    next_conditional_exp = parse_conditional_exp(tokens);
                    return ASTTree::CondExp(Box::new(logical_or_exp),
                        Box::new(next_exp),
                        Box::new(next_conditional_exp));
                },
                _ => panic!("parse_conditional_exp fail")
            }
        },
        // Ternary optional
        _ => {
            return logical_or_exp;
        }
    }
}

/*
    Grammar:
   <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
*/
fn parse_logical_or(tokens: &mut VecDeque<Token>) -> ASTTree {
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
    let mut eq_exp = parse_bitor_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_eq_exp;

    while next == Some(&Token::TAnd) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_eq_exp = parse_bitor_exp(tokens);
        eq_exp = ASTTree::BinaryOp(Box::new(eq_exp), op, Box::new(next_eq_exp));
        next = tokens.get(0);
    }

    return eq_exp;
}

/*
    Grammar:
    <bitor-exp> ::= <bitxor-exp> { "|" <bitxor-exp> }
*/
fn parse_bitor_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut bitor_exp = parse_bitxor_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_bitor_exp;

    while next == Some(&Token::TBitOr) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_bitor_exp"),
        };
        next_bitor_exp = parse_bitxor_exp(tokens);
        bitor_exp = ASTTree::BinaryOp(Box::new(bitor_exp), op,
            Box::new(next_bitor_exp));
        next = tokens.get(0);
    }
    return bitor_exp;
}

/*
    Grammar:
    <bitxor-exp> ::= <bitand-exp> { "^" <bitand-exp> }
*/
fn parse_bitxor_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut bitxor_exp = parse_bitand_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_bitxor_exp;

    while next == Some(&Token::TXor) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_bitxor_exp"),
        };
        next_bitxor_exp = parse_bitand_exp(tokens);
        bitxor_exp = ASTTree::BinaryOp(Box::new(bitxor_exp), op,
            Box::new(next_bitxor_exp));
        next = tokens.get(0);
    }
    return bitxor_exp;
}

/*
    Grammar:
    <bitand-exp> ::= <equality-exp> { "&" <equality-exp> }
*/
fn parse_bitand_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut bitand_exp = parse_eq_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_bitand_exp;

    while next == Some(&Token::TBitAnd) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_bitand_exp"),
        };
        next_bitand_exp = parse_eq_exp(tokens);
        bitand_exp = ASTTree::BinaryOp(Box::new(bitand_exp), op,
            Box::new(next_bitand_exp));
        next = tokens.get(0);
    }
    return bitand_exp;
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
    <relational-exp> ::= <bitshift-exp> { ("<" | ">" | "<=" | ">=") <bitshift-exp> }
*/
fn parse_relational_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut bitshift_exp = parse_bitshift_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_bitshift_exp;

    while next == Some(&Token::TLess) || next == Some(&Token::TLeq)
    || next == Some(&Token::TGreater) || next == Some(&Token::TGeq) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_logical_and")
        };
        next_bitshift_exp = parse_bitshift_exp(tokens);
        bitshift_exp = ASTTree::BinaryOp(Box::new(bitshift_exp), op, 
            Box::new(next_bitshift_exp));
        next = tokens.get(0);
    }
    return bitshift_exp;
}

/*
    Grammar:
    <binshift-exp> ::= <additive-exp> { (">>" | "<<") <additive-exp> }
*/
fn parse_bitshift_exp(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut add_exp = parse_additive_exp(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_add_exp;

    while next == Some(&Token::TLShift) || next == Some(&Token::TRShift) {
        op = match tokens.pop_front() {
            Some(inner) => inner,
            None => panic!("failed parse_bitshift_exp"),
        };
        next_add_exp = parse_additive_exp(tokens);
        add_exp = ASTTree::BinaryOp(Box::new(add_exp), op, 
            Box::new(next_add_exp));
        next = tokens.get(0);
    }
    return add_exp;
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
    <term> ::= <factor> { ("*" | "/" | "%" ) <factor> }
*/
pub fn parse_term(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut factor = parse_factor(tokens);

    let mut next = tokens.get(0);
    let mut op;
    let mut next_factor;

    while is_bin_term_op(next) {
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
    <factor> ::= <function-call> | "(" <exp> ")" | <int> | <id> | <unary_op> <factor>
*/
pub fn parse_factor(tokens: &mut VecDeque<Token>) -> ASTTree {
    let tkn = tokens.pop_front();
    match tkn {
        // <factor> ::= "(" <exp> ")"
        Some(Token::TOpenParen) => {
            let exp = parse_exp(tokens);
            chk_token(tokens, Token::TCloseParen, "parse_factor, close paren");
            return exp;
        },
        // <factor> ::= <int>
        Some(Token::TIntLit(x)) => {
            return ASTTree::Constant(x);
        },
        // <factor> ::= <id>
        Some(Token::TIdentifier(id)) => {
            // <function-call>
            if tokens.get(0) == Some(&Token::TOpenParen) {
                // Mega scuffed
                tokens.push_front(Token::TIdentifier(id));
                return parse_function_call(tokens);
            }
            return ASTTree::Var(id);
        },
        Some(inner) => {
            // <factor> ::= <unary_op> <factor>
            if is_un_op(&inner) {
                let factor = parse_factor(tokens);
                return ASTTree::UnaryOp(inner, Box::new(factor));
            } else {
                panic!("Parse factor fail 1");
            };
        },
        _ => panic!("parse_factor fail"),
    }
}


/*
    Grammar:
    <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
*/
pub fn parse_function_call(tokens: &mut VecDeque<Token>) -> ASTTree {
    let func_id = extract_id(tokens.pop_front());
    chk_token(tokens, Token::TOpenParen, "parse_function_call open parens fail");
    
    let next = tokens.get(0);

    // <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"

    if next == Some(&Token::TCloseParen) {
        // Case: id "(" ")"
        return ASTTree::FuncCall(func_id, Vec::new());
    }

    let mut exp_list: Vec<Box<ASTTree>> = Vec::new();
    exp_list.push(Box::new(parse_exp(tokens)));

    while tokens.get(0) != Some(&Token::TCloseParen) {
        if tokens.pop_front() != Some(Token::TComma) {
            panic!("parse_function_call comma fail");
        }
        exp_list.push(Box::new(parse_exp(tokens)));
    }

    // Pop off parens tokens
    if tokens.pop_front() != Some(Token::TCloseParen) {
        panic!("parse_function_call close parens fail");
    }
    return ASTTree::FuncCall(func_id, exp_list);
}

/*
    Grammar:
    <block-item> ::= <statement> | <declaration>
*/
fn parse_block_item(tokens: &mut VecDeque<Token>) -> ASTTree {
    match tokens.get(0) {
        // <block-item> ::= <declaration>
        Some(Token::TInt) => {
            let decl = parse_declare(tokens);
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
    <function> ::= "int" <id> "(" [ "int" <id> { "," "int" <id> } ] ")" ( "{" { <block-item> } "}" | ";" )
*/
pub fn parse_function(tokens: &mut VecDeque<Token>) -> ASTTree {
    if tokens.pop_front() != Some(Token::TInt) {
        panic!("Parse function type fail")
    }

    let func_id;
    match tokens.pop_front() {
        Some(Token::TIdentifier(tkn)) => func_id = tkn,
        _ => panic!("Parse function id fail"),
    }

    // Parens 
    chk_token(tokens, Token::TOpenParen, "parse_function, open paren");

    let next = tokens.get(0);

    if next == Some(&Token::TCloseParen) {
        tokens.pop_front();

        match tokens.get(0) {
            // Case: "int" <id" "(" ")" ";"
            Some(&Token::TSemicolon) => return ASTTree::Function(func_id, Vec::new(), None),
            // Case: "int" <id> "(" ")" "{" { <block-item> } "}"
            Some(&Token::TOpenBrace) => {
                let func_body = parse_compound(tokens);
                return ASTTree::Function(func_id, Vec::new(), Some(Box::new(func_body)));
            },
            _ => panic!("parse_function fail"),
        };
    } else {
        let mut id_list: Vec<String> = Vec::new();

        // Case: "int" <id> "(" [ "int" <id> { "," "int" <id> } ] ")" ( "{" { <block-item> } "}" | ";" )
        while tokens.get(0) == Some(&Token::TInt) {
            tokens.pop_front();
            match tokens.pop_front() {
                Some(Token::TIdentifier(arg_id)) => id_list.push(arg_id),
                _ => panic!("parse_function args id fail"),
            };

            let next_tkn = tokens.pop_front();

            if next_tkn != Some(Token::TCloseParen) && next_tkn != Some(Token::TComma) {
                panic!("parse_function parse args fail")
            }
            if next_tkn == Some(Token::TCloseParen) {
                break
            }
        }
        
        // Could possibly end with just semicolon
        if tokens.get(0) == Some(&Token::TSemicolon) {
            tokens.pop_front();
            return ASTTree::Function(func_id, id_list, None);
        } else {
            // Parse rest of function body
            let func_body = parse_compound(tokens);
            return ASTTree::Function(func_id, id_list, Some(Box::new(func_body)));
        }
    }
}

pub fn parse_program(tokens: &mut VecDeque<Token>) -> ASTTree {
    let mut func_list: Vec<Box<ASTTree>> = Vec::new();
    let mut func_decl;
    while !tokens.is_empty() {
        func_decl = parse_function(tokens);
        func_list.push(Box::new(func_decl));
    }
    return ASTTree::Program(func_list);
}

pub fn parser(tkn_stack: Vec<Token>) -> ASTTree {
    let mut tokens: VecDeque<Token> = VecDeque::from_iter(tkn_stack);
    let tree = parse_program(&mut tokens);

    return tree;
}