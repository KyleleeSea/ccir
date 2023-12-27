mod types;
mod lex;
mod parse;
mod gen;
mod debug;

fn main() {
    let tkn_stack = lex::lexer();
    debug::print_tkn_vec(tkn_stack);

}
