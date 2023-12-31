use std::fs::OpenOptions;
use std::fs::File;
use std::io::Write;
use super::types::ASTTree;
use super::types::Token;
use std::fs;
use std::env;
use std::path::Path;
use std::process::Command;

pub fn generate(tree: ASTTree) {
    let args: Vec<String> = env::args().collect();

    if Path::new("assembly.s").exists() {
      fs::remove_file("assembly.s").unwrap();
    }

    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("assembly.s")
        .unwrap();
    
    match tree {
        ASTTree::Program(child) => process_function(*child, file),
        _ => invalid_match("generate"),
    }

    Command::new("gcc")
    .arg("assembly.s")
    .arg("-o")
    .arg(&args[2])
    .output()
    .expect("Failed to execute command");
}

fn process_function(tree: ASTTree, mut file: File) {
    match tree {
        ASTTree::Function(id, _type, child) => {
            write_wrapper(write!(file, ".globl _{}\n", id));
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

fn process_expression(tree: ASTTree, mut file: &File) {
    match tree {
        ASTTree::Constant(x) => {
            write_wrapper(write!(file, "movl ${}, %eax\n", x));
        },
        ASTTree::UnaryOp(op, child) => {
            process_expression(*child, &file);
            match op {
                Token::TNeg => write_wrapper(write!(file, "neg %rax\n")),
                Token::TBitComp => write_wrapper(write!(file, "not %rax\n")),
                Token::TLNeg => {
                    write_wrapper(write!(file, "cmpq $0, %rax\n"));
                    write_wrapper(write!(file, "movq $0, %rax\n"));
                    write_wrapper(write!(file, "sete %al\n"));
                },
                _ => panic!("Invalid operator found in unaryOp code gen"),
            }
        },
        ASTTree::BinaryOp(left, op, right) => {
            process_expression(*left, &file);
            write_wrapper(write!(file, "push %rax\n"));
            process_expression(*right, &file);
            // e1 in rcx, e2 in rax
            write_wrapper(write!(file, "pop %rcx\n"));
            match op {
                Token::TAdd => write_wrapper(write!(file, "addl %rcx, %rax\n")),
                Token::TMultiply =>
                    write_wrapper(write!(file, "imul %rcx, %rax\n")),
                // subl src, dst... eax is e2 rcx is e1
                Token::TNeg => write_wrapper(write!(file, "subl %rax, %rcx\n")),
                Token::TDivide => {
                    // Move e2 into rbx
                    write_wrapper(write!(file, "movl %rax, %rbx\n"));
                    // Move e1 to rax
                    write_wrapper(write!(file, "movl %rcx, %rax\n"));
                    // Sign extend into rdx
                    write_wrapper(write!(file, "cdq\n"));
                    write_wrapper(write!(file, "idivl %rbx\n"));
                },
                _ => panic!("Invalid operator found in binaryOp code gen"),
            }

        }
        _ => invalid_match("process return"),
    }
}

fn process_return(tree: ASTTree, mut file: File) {
    process_expression(tree, &file);
    write_wrapper(write!(file, "{}", "ret\n"));
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