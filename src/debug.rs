use super::types;

pub fn print_tkn_vec(tkn_stack: &mut Vec<types::Token>) {
    for el in tkn_stack {
        match el {
            types::Token::TOpenBrace => print!("open brace "),
            types::Token::TCloseBrace => print!("close brace "),
            types::Token::TOpenParen => print!("( "),
            types::Token::TCloseParen => print!(") "),
            types::Token::TSemicolon => print!("; "),
            types::Token::TInt => print!("int "),
            types::Token::TReturn => print!("return "),
            types::Token::TIdentifier(inner_str) => print!("{} ", inner_str),
            types::Token::TIntLit(inner_num) => print!("{} ", inner_num),
            types::Token::TNeg => print!("- "),
            types::Token::TBitComp => print!("~ "),
            types::Token::TLNeg => print!("! "),
            types::Token::TAdd => print!("+ "),
            types::Token::TMultiply => print!("* "),
            types::Token::TDivide => print!("/ "),
            types::Token::TBitAnd => print!("& "),
            types::Token::TBitOr => print!("| "),
            types::Token::TXor => print!("^ "),
            types::Token::TLShift => print!("<< "),
            types::Token::TRShift => print!(">> "),
            types::Token::TAnd => print!("&& "),
            types::Token::TOr => print!("|| "),
            types::Token::TAssign => print!("= "),
            types::Token::TEq => print!("== "),
            types::Token::TNeq => print!("!= "),
            types::Token::TLess => print!("< "),
            types::Token::TLeq => print!("<= "),
            types::Token::TGreater => print!("> "),
            types::Token::TGeq => print!(">= "),
            types::Token::TMod => print!("% "),
            types::Token::TColon => print!(": "),
            types::Token::TIf => print!("if "),
            types::Token::TElse => print!("else "),
            types::Token::TQuestion => print!("? "),
            types::Token::TFor => print!("for "),
            types::Token::TWhile => print!("while "),
            types::Token::TDo => print!("do "),
            types::Token::TBreak => print!("break "),
            types::Token::TContinue => print!("continue "),
            types::Token::TComma => print!(", "),
        }
    }
}

pub fn print_ast(tree: types::ASTTree) {
    match tree {
        types::ASTTree::Function(func_id, func_type, child) => {
            print!("function: {} ", func_id);
            if func_type == types::Token::TInt {
                println!("int ");
            }
            print_ast(*child);
        },
        types::ASTTree::Constant(cnst) => {
            println!("const: {} ", cnst)
        },
        types::ASTTree::Return(child) => {
            println!("return:");
            print_ast(*child);
        },
    }
}