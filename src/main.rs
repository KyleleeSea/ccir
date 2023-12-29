mod types;
mod lex;
mod parse;
mod gen;
mod debug;

fn main() {
    let mut tkn_stack = lex::lexer();
    // debug::print_tkn_vec(&mut tkn_stack);
    parse::parser(tkn_stack);
}
