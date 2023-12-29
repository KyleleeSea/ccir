use std::fs::OpenOptions;
use std::fs::File;
use std::io::Write;
use super::types::ASTTree;

pub fn generate(tree: ASTTree) {
    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("assembly.s")
        .unwrap();
    
    match tree {
        ASTTree::Program(child) => process_function(*child, file),
        _ => invalid_match("generate"),
    }

    // run commands here
    
}

fn process_function(tree: ASTTree, mut file: File) {
    match tree {
        ASTTree::Function(id, _type, child) => {
            // let header = format!(".globl _{}\n", id).as_str();
            write_wrapper(write!(file, ".globl _{}\n", id));
            // let name = format!("_{}:\n", id).as_str();
            write_wrapper(write!(file, "_{}:\n", id));
            process_statement(*child, file);
        },
        _ => invalid_match("process_function"),
    }
}

fn process_statement(tree: ASTTree, file: File) {
    match tree {
        ASTTree::Statement(child) => match *child {
            ASTTree::Return(inner_child) => process_return(*inner_child, file),
            _ => invalid_match("process_statement inner"),
        },
        _ => invalid_match("process_statement"),
    }
}

fn process_return(tree: ASTTree, mut file: File) {
    match tree {
        ASTTree::Constant(x) => {
            // let move_instr = format!("movl  ${}, %eax\n", x).as_str();
            write_wrapper(write!(file, "movl ${}, %eax\n", x));
            write_wrapper(write!(file, "{}", "ret\n"));
        },
        _ => invalid_match("process return"),
    }
}

fn write_wrapper(res: Result<(), std::io::Error>) {
    match res {
        Ok(_file) => (),
        Err(e) => panic!("Error: {}", e),
    }
}

fn invalid_match(name: &str) {
    panic!("code gen failed on {}", name);
}