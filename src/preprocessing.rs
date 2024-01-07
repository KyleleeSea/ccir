use super::types::ASTTree;
use super::types::Token;
use std::vec::Vec;

pub fn eval_global_constants(tree: ASTTree) -> ASTTree {
    match tree {
        ASTTree::Program(children) => {
            let mut updated_children : Vec<Box<ASTTree>> = Vec::new();

            for child in children {
                match *child {
                    ASTTree::Declare(id, body_opt) => {
                        match body_opt {
                            Some(body) => {
                                let new_constant : i64 = eval_global_exp(*body);
                                let node = Box::new(ASTTree::Declare
                                    (id.clone(), Some(Box::new(ASTTree::Constant
                                        (new_constant)))));
                                updated_children.push(node);
                            },
                            None => {
                                let node = Box::new(ASTTree::Declare(id.clone(),
                            None));
                                updated_children.push(node);
                            }
                        }
                    },
                    _ => updated_children.push(child),
                }
            }

        return ASTTree::Program(updated_children);
        },
        _ => panic!("non program found in preprocessing"),
    }
}

fn eval_global_exp(tree: ASTTree) -> i64 {
    match tree {
        ASTTree::Constant(x) => x,
        ASTTree::BinaryOp(left, tkn, right) => {
            let left_value = eval_global_exp(*left);
            let right_value = eval_global_exp(*right);

            match tkn {
                Token::TAdd => return left_value + right_value,
                Token::TMultiply => return left_value * right_value,
                Token::TDivide => return left_value / right_value,
                Token::TMod => return left_value % right_value,
                Token::TBitAnd => return left_value & right_value,
                Token::TBitOr => return left_value | right_value,
                Token::TLShift => return left_value << right_value,
                Token::TRShift => return left_value >> right_value,
                _ => panic!("unsupported binary op found in global expression"),
            }

        },
        ASTTree::UnaryOp(tkn, child) => {
            let child_value = eval_global_exp(*child);
            match tkn {
                Token::TNeg => return -child_value,
                Token::TLNeg => {
                    if child_value == 0 {
                        return 1;
                    }
                    else {
                        return 0;
                    }
                },
                Token::TBitComp => return !child_value,
                _ => panic!("unsupported unary op found in global expression"),
            }

        },
        _ => panic!("non constant found in global expression, 
            preprocessing fail")
    }
}