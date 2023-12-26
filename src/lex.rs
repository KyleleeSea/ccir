use std::env;
use std::fs;
use std::vec::Vec;
use super::types;

pub fn lexer() {
    // Read command line file
    let args: Vec<String> = env::args().collect();

    let file_path = &args[1];
    
    let mut contents = fs::read_to_string(file_path)
        .expect("Should have been able to read the file");
    
    // Skip all whitespace and newlines by stripping immediately
    contents.retain(|c| !c.is_whitespace());

    // let mut tknStack = Vec::new();

    // tokenize(contents, tknStack);

    // println!("With text:\n{contents}");
}

// fn tokenize(contents: String, tknStack: Vec<types::Token>) {
//     let mut chars = contents.chars();

//     for c in chars {
//         println!("next {}", chars.next().unwrap());
//     }
// }

// fn tokenize_single_token(contents: String, tkn_stack: Vec, prev: TknAcc) {

// }