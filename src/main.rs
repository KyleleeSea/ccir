mod types;
mod lex;
mod parse;
mod gen;
mod debug;
mod preprocessing;

fn main() {
    let tkn_stack = lex::lexer();
    // debug::print_tkn_vec(&mut tkn_stack);
    let mut tree = parse::parser(tkn_stack);
    tree = preprocessing::eval_global_constants(tree);
    // debug::print_ast(tree);
    gen::generate(tree);
}
