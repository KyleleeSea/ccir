use super::types::Token;
use super::types::ASTTree;

pub fn print_tkn_vec(tkn_stack: &mut Vec<Token>) {
    for el in tkn_stack {
        match el {
            Token::TOpenBrace => print!("open brace "),
            Token::TCloseBrace => print!("close brace "),
            Token::TOpenParen => print!("( "),
            Token::TCloseParen => print!(") "),
            Token::TSemicolon => print!("; "),
            Token::TInt => print!("int "),
            Token::TReturn => print!("return "),
            Token::TIdentifier(inner_str) => print!("{} ", inner_str),
            Token::TIntLit(inner_num) => print!("{} ", inner_num),
            Token::TNeg => print!("- "),
            Token::TBitComp => print!("~ "),
            Token::TLNeg => print!("! "),
            Token::TAdd => print!("+ "),
            Token::TMultiply => print!("* "),
            Token::TDivide => print!("/ "),
            Token::TBitAnd => print!("& "),
            Token::TBitOr => print!("| "),
            Token::TXor => print!("^ "),
            Token::TLShift => print!("<< "),
            Token::TRShift => print!(">> "),
            Token::TAnd => print!("&& "),
            Token::TOr => print!("|| "),
            Token::TAssign => print!("= "),
            Token::TEq => print!("== "),
            Token::TNeq => print!("!= "),
            Token::TLess => print!("< "),
            Token::TLeq => print!("<= "),
            Token::TGreater => print!("> "),
            Token::TGeq => print!(">= "),
            Token::TMod => print!("% "),
            Token::TColon => print!(": "),
            Token::TIf => print!("if "),
            Token::TElse => print!("else "),
            Token::TQuestion => print!("? "),
            Token::TFor => print!("for "),
            Token::TWhile => print!("while "),
            Token::TDo => print!("do "),
            Token::TBreak => print!("break "),
            Token::TContinue => print!("continue "),
            Token::TComma => print!(", "),
        }
    }
}

pub fn print_ast(tree: ASTTree) {
    match tree {
        ASTTree::Program(childlist) => {
            for prog in childlist {
                print_ast(*prog);
            }
        },
        ASTTree::Function(func_id, func_args, children_opt) => {
            print!("function: {} ", func_id);

            for varname in func_args {
                print!("arg: {} ", varname);
            }

            match children_opt {
                None => print!("function declaration, no body\n"),
                Some(body) => {
                    print!("body: ");
                    print_ast(*body);
                }
            }
        },
        ASTTree::FuncCall(func_id, exp_list) => {
            print!("func call: {} ", func_id);

            for child in exp_list {
                print_ast(*child);
            }
        }
        ASTTree::Constant(cnst) => {
            println!("const: {} ", cnst)
        },
        ASTTree::Return(child) => {
            println!("return:");
            print_ast(*child);
        },
        ASTTree::Statement(child) => {
            print!("statement... ");
            print_ast(*child);
        },
        ASTTree::UnaryOp(tkn, child) => {
            print!("(");
            match tkn {
                Token::TNeg => print!("op - "),
                Token::TBitComp => print!("op ~ "),
                Token::TLNeg => print!("op ! "),
                _ => print!("unidentified unary op among us"),
            }
            print_ast(*child);
            print!(")\n");
        },
        ASTTree::BinaryOp(left, tkn, right) => {
            print!("(");
            print_ast(*left);
            match tkn {
                Token::TNeg => print!("binary op - "),
                Token::TAdd => print!("op + "),
                Token::TMultiply => print!("op * "),
                Token::TDivide => print!("op / "),
                Token::TAnd => print!("&& "),
                Token::TOr => print!("|| "),
                Token::TEq => print!("== "),
                Token::TNeq => print!("!= "),
                Token::TLess => print!("< "),
                Token::TLeq => print!("<= "),
                Token::TGreater => print!("> "),
                Token::TGeq => print!(">= "),
                Token::TMod => print!("% "),
                _ => print!("unidentified binary op among us"),
            }
            print_ast(*right);
            print!(")\n");
        },  
        ASTTree::Declare(id, exp) => {
            print!("dec {} ", id);
            match exp {
                Some(child) => {
                        print!("= ");
                        print_ast(*child);
                    },
                None => (),
            }
        },
        ASTTree::Var(id) => print!("var {} ", id),
        ASTTree::Assign(id, child) => {
            print!("assign {}", id);
            print_ast(*child);
        },
        ASTTree::BlockItem(child) => {
            print!("block item... ");
            print_ast(*child);
        },
        ASTTree::Conditional(cond, if_statement, else_statement) => {
            print!("if ");
            print_ast(*cond);
            print!("then ");
            print_ast(*if_statement);
            match else_statement {
                None => (),
                Some(inner) => {
                    print!("else ");
                    print_ast(*inner);
                },
            };
        },
        ASTTree::Compound(block_item_list) => {
            print!("compound_start ");
            for block_item in block_item_list {
                print_ast(*block_item);
            };
            print!("compound_end ");
        },
        ASTTree::CondExp(e1, e2, e3) => {
            print!("ternary ");
            print_ast(*e1);
            print!("? ");
            print_ast(*e2);
            print!(": ");
            print_ast(*e3);
        },
        ASTTree::For(initopt, control, postopt, body) => {
            print!("\n");
            print!("for ");
            match initopt {
                None => print!("no init "),
                | Some(init) => {
                    print!("init: ");
                    print_ast(*init);
                },
            };
            print!("control: ");
            print_ast(*control);
            match postopt {
                None => print!("no postexp "),
                | Some(postexp) => {
                    print!("postexp: ");
                    print_ast(*postexp);
                },
            };
            print!("body: ");
            print_ast(*body);
            print!("\n");
        },
        ASTTree::ForDecl(decl, control, postopt, body) => {
            print!("\n");
            print!("for decl ");
            print_ast(*decl);
            print!("control: ");
            print_ast(*control);
            match postopt {
                None => print!("no postexp "),
                | Some(postexp) => {
                    print!("postexp: ");
                    print_ast(*postexp);
                },
            };
            print!("body: ");
            print_ast(*body);
            print!("\n");
        },
        ASTTree::While(condition, body) => {
            print!("\n");
            print!("while: condition ");
            print_ast(*condition);
            print!("body ");
            print_ast(*body);
            print!("\n");
        },
        ASTTree::Do(body, condition) => {
            print!("\n");
            print!("while: body ");
            print_ast(*body);
            print!("condition ");
            print_ast(*condition);
            print!("\n");
        },
        ASTTree::Break => {
            print!("break ");
        },
        ASTTree::Continue => {
            print!("continue ");
        },
        ASTTree::NullExp => {
            print!("null exp ");
        },
    }
}