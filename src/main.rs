mod types;
mod lex;
mod parse;
mod gen;

fn main() {
    lex::lexer();
    // types::types_test();
    // lex::lex_test();
    // parse::parse_test();
    // gen::gen_test();
}

// use std::env;
// use std::fs;

// enum Token {
//     TPLUS,
//     TMINUS,
//     TSTAR,
//     TSLASH,
//     TINTLIT(i64)
// }

// fn main() {
//     let args: Vec<String> = env::args().collect();

//     let file_path = &args[1];
    
//     let contents = fs::read_to_string(file_path)
//         .expect("Should have been able to read the file");

//     let 

//     println!("With text:\n{contents}");
// }


