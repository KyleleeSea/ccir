use std::fs::OpenOptions;
use std::fs::File;
use std::io::Write;
use super::types::ASTTree;
use super::types::Token;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::collections::HashMap;

fn write_prologue(mut file: &File) {
    write_wrapper(write!(file, "push %rbp\n"));
    write_wrapper(write!(file, "movq %rsp, %rbp\n"));
}

fn write_epilogue(mut file: &File) {
    write_wrapper(write!(file, "movq %rbp, %rsp\n"));
    write_wrapper(write!(file, "pop %rbp\n"));
}

fn write_wrapper(res: Result<(), std::io::Error>) {
    match res {
        Ok(_file) => (),
        Err(e) => panic!("Error: {}", e),
    }
}

fn get_unique_label(label_counter: &mut i32) -> String {
    let label = format!("_label{}", label_counter);
    *label_counter = *label_counter + 1;
    return label;
}

fn invalid_match(name: &str) {
    panic!("code gen failed on {}", name);
}

pub fn generate(tree: ASTTree) {
    if Path::new("assembly.s").exists() {
      fs::remove_file("assembly.s").unwrap();
    }

    let file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("assembly.s")
        .unwrap();

    let mut label_counter : i32 = 0;
    
    match tree {
        ASTTree::Program(child) => process_function(*child, file, &mut
        label_counter),
        _ => invalid_match("generate"),
    }

    Command::new("gcc")
    .arg("assembly.s")
    .arg("-o")
    .arg("out")
    .output()
    .expect("Failed to execute command");
}

fn process_function(tree: ASTTree, mut file: File, label_counter: &mut i32) {
    match tree {
        ASTTree::Function(id, _type, children) => {
            write_wrapper(write!(file, ".globl _{}\n", id));
            write_wrapper(write!(file, "_{}:\n", id));
            write_prologue(&file);

            let mut stack_ind : i32 = -8;
            let mut var_map = HashMap::new();

            for child in children {
                (stack_ind, var_map) = process_block(*child, &file, stack_ind, 
                    var_map, label_counter);
            }


        },
        _ => invalid_match("process_function"),
    }
}

fn process_block(tree: ASTTree, file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
    -> (i32, HashMap<String, i32>) {

    match tree {
        ASTTree::BlockItem(child) => match *child {
            ASTTree::Declare(ref _id, ref _inner_child) => 
            (stack_ind, var_map) = process_declare(*child, file, stack_ind,
                 var_map, label_counter),

            _ => {
                process_statement(*child, file, stack_ind, var_map.clone(), 
            label_counter);
            // unit required to make match arms have same type
            ();
        },
        },
        _ => panic!("process_block failed"),
    };

    return (stack_ind, var_map);
}

// Statement can be Return, Declare, or an arbitrary expression
fn process_statement(tree: ASTTree, file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
    -> (i32, HashMap<String, i32>) {
    match tree {
        ASTTree::Statement(child) => match *child {
            ASTTree::Return(inner_child) => process_return(*inner_child, file, 
                stack_ind, var_map.clone(), label_counter),

            ASTTree::Conditional(ref _condition, ref _s1, ref _s2) => 
            (stack_ind, var_map) = process_conditional(*child, file, stack_ind, 
                var_map, label_counter),

            _ => process_expression(*child, file, stack_ind, var_map.clone(), 
            label_counter),
        },
        _ => panic!("failed process_statement"),
    };

    return (stack_ind, var_map);
}

fn process_conditional(tree: ASTTree, mut file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
    -> (i32, HashMap<String, i32>)  {
        match tree {
            ASTTree::Conditional(condition, s1, s2_opt) => {
                match s2_opt {
                    None => {
                        let label_a = get_unique_label(label_counter);

                        process_expression(*condition, file, stack_ind, 
                            var_map.clone(), label_counter);
                        write_wrapper(write!(file, "cmpq $0, %rax\n"));
                        write_wrapper(write!(file, "je {}\n", label_a));

                        (stack_ind, var_map) = process_statement(*s1, file, 
                            stack_ind, var_map, label_counter);
                        
                        write_wrapper(write!(file, "{}:\n", label_a));

                    },

                    Some(s2) => {
                        let label_a = get_unique_label(label_counter);
                        let label_b = get_unique_label(label_counter);

                        process_expression(*condition, file, stack_ind, 
                            var_map.clone(), label_counter);

                        write_wrapper(write!(file, "cmpq $0, %rax\n"));
                        write_wrapper(write!(file, "je {}\n", label_a));

                        (stack_ind, var_map) = process_statement(*s1, file, 
                            stack_ind, var_map, label_counter);
                        write_wrapper(write!(file, "jmp {}\n", label_b));

                        write_wrapper(write!(file, "{}:\n", label_a));
                        (stack_ind, var_map) = process_statement(*s2, file, 
                            stack_ind, var_map, label_counter);
                        write_wrapper(write!(file, "{}:\n", label_b));

                    },
                }
            },
            _ => invalid_match("process_conditional"),
        };

        return (stack_ind, var_map)
}

fn process_expression(tree: ASTTree, mut file: &File, stack_ind: i32, 
    var_map: HashMap<String, i32>, label_counter: &mut i32)
    {
    match tree {
        ASTTree::Constant(x) => {
            write_wrapper(write!(file, "movq ${}, %rax\n", x));
        },
        ASTTree::UnaryOp(op, child) => {
            process_expression(*child, file, stack_ind, 
                var_map, label_counter);
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
        ASTTree::BinaryOp(left, op, right) => 
        process_binary_op(*left, op, *right, file, 
            stack_ind, var_map, label_counter),

        ASTTree::Var(id) => {
            match var_map.get(&id) {
                None => panic!("Var referenced without declaration"),
                Some(&offset) => {
                    write_wrapper(write!(file, "movq {}(%rbp), %rax\n", 
                offset));
                },
            }
        },

        ASTTree::Assign(id, child) => {
            process_expression(*child, file, stack_ind, 
                var_map.clone(), label_counter);
            match var_map.get(&id) {
                None => panic!("Var assignment without declaration"),
                Some(&offset) => {
                    write_wrapper(write!(file, "movq %rax, {}(%rbp)\n", 
                offset));
                },
            }     
        }

        _ => invalid_match("process exp"),
    }
}

fn write_arithmetic_partial(left: ASTTree, op: Token, right: ASTTree,  
    mut file: &File, stack_ind: i32,  var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {

    process_expression(left, file, stack_ind, var_map.clone(), label_counter);
    write_wrapper(write!(file, "push %rax\n"));
    process_expression(right, file, stack_ind, var_map, label_counter);
    // e1 in rcx, e2 in rax
    write_wrapper(write!(file, "pop %rcx\n"));
}

fn write_relational_partial(left: ASTTree, op: Token, right: ASTTree,  
    mut file: &File, stack_ind: i32,  var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {

    process_expression(left, file, stack_ind, var_map.clone(), label_counter);
    write_wrapper(write!(file, "push %rax\n"));
    process_expression(right, file, stack_ind, var_map, label_counter);
    // e1 in rcx, e2 in rax
    write_wrapper(write!(file, "pop %rcx\n"));
    write_wrapper(write!(file, "cmpq %rax, %rcx\n"));
    write_wrapper(write!(file, "movq $0, %rax\n"));

}

fn process_binary_op(left: ASTTree, op: Token, right: ASTTree,  mut file: &File, 
    stack_ind: i32,  var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {

    match op {
        Token::TAdd => {
            write_arithmetic_partial(left, op, right, file, stack_ind,
            var_map, label_counter);
            write_wrapper(write!(file, "addq %rcx, %rax\n"));
        },
        
        Token::TMultiply => {
            write_arithmetic_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "imul %rcx, %rax\n"));
        },

        Token::TBitAnd => {
            write_arithmetic_partial(left, op, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "and %rcx, %rax\n"));
        }

        Token::TBitOr => {
            write_arithmetic_partial(left, op, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "or %rcx, %rax\n"));
        }

        Token::TXor => {
            write_arithmetic_partial(left, op, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "xorq %rcx, %rax\n"));
        }

        Token::TRShift => {
            write_arithmetic_partial(left, op, right, file, stack_ind, 
                var_map, label_counter);
            // e1 = %rcx, e2 = %rax
            write_wrapper(write!(file, "push %rax\n"));
            write_wrapper(write!(file, "movq %rcx, %rax\n"));
            write_wrapper(write!(file, "pop %rcx\n"));
            write_wrapper(write!(file, "sarq %cl, %rax\n"));
        }

        Token::TLShift => {
            write_arithmetic_partial(left, op, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "push %rax\n"));
            write_wrapper(write!(file, "movq %rcx, %rax\n"));
            write_wrapper(write!(file, "pop %rcx\n"));
            write_wrapper(write!(file, "shl %cl, %rax\n"));
        }

        Token::TNeg => {
            write_arithmetic_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            // want e1 in rax, e2 in ecx
            write_wrapper(write!(file, "movq %rcx, %rdx\n"));
            // e1 in rbx, e2 in rax
            write_wrapper(write!(file, "movq %rax, %rcx\n"));
            // e1 in rbx, e2 in rcx
            write_wrapper(write!(file, "movq %rdx, %rax\n"));
            // e1 in rax, e2 in rcx
            write_wrapper(write!(file, "subq %rcx, %rax\n"));
        },
        Token::TDivide => {
            write_arithmetic_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            // Move e2 into r8 
            write_wrapper(write!(file, "movq %rax, %r8\n"));
            // Move e1 to rax
            write_wrapper(write!(file, "movq %rcx, %rax\n"));
            // Sign extend into rdx
            write_wrapper(write!(file, "cqo\n"));
            // Interesting behavior... %r8 is supposed to be 
            // the dst, but the result always goes into %rax
            // when I run on lldb...
            write_wrapper(write!(file, "idivq %r8\n"));
        },
        Token::TMod => {
            write_arithmetic_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            // e1 in rcx, e2 in rax

            // Using divq S:
            // Unsigned divide %rdx:%rax by S
            // Quotient stored in %rax
            // Remainder stored in %rdx
            
            // Temporarily store %rdx, %r8 on stack
            // Top of stack in order: %rdx, %r8
            write_wrapper(write!(file, "push %r8\n"));
            write_wrapper(write!(file, "push %rdx\n"));
            write_wrapper(write!(file, "movq %rax, %r8\n"));

            // Clear out %rdx, %rcx -> %rax
            write_wrapper(write!(file, "xorq %rdx, %rdx\n"));
            write_wrapper(write!(file, "movq %rcx, %rax\n"));

            // %rdx:%rax div %r8 
            write_wrapper(write!(file, "divq %r8\n"));
            // Move remainder to %rax
            write_wrapper(write!(file, "movq %rdx, %rax\n"));

            // Restore %rdx, %r8
            write_wrapper(write!(file, "pop %rdx\n"));
            write_wrapper(write!(file, "pop %r8\n"));
        }
        Token::TAnd => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);
            process_expression(left, file, stack_ind, var_map.clone(), 
            label_counter);
            write_wrapper(write!(file, "cmpq $0, %rax\n"));
            write_wrapper(write!(file, "jne {}\n", label_a));
            write_wrapper(write!(file, "jmp {}\n", label_b));

            write_wrapper(write!(file, "{}:\n", label_a));
            process_expression(right, file, stack_ind, var_map, label_counter);
            write_wrapper(write!(file, "cmpq $0, %rax\n"));
            write_wrapper(write!(file, "movq $0, %rax\n"));
            write_wrapper(write!(file, "setne %al\n"));

            write_wrapper(write!(file, "{}:\n", label_b));
        },
        Token::TOr => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);
            process_expression(left, file, stack_ind, var_map.clone(), 
            label_counter);
            write_wrapper(write!(file, "cmpq $0, %rax\n"));
            write_wrapper(write!(file, "je {}\n", label_a));
            write_wrapper(write!(file, "movq $1, %rax\n"));
            write_wrapper(write!(file, "jmp {}\n", label_b));
            
            write_wrapper(write!(file, "{}:\n", label_a));
            process_expression(right, file, stack_ind, var_map, label_counter);
            write_wrapper(write!(file, "cmpq $0, %rax\n"));
            write_wrapper(write!(file, "movq $0, %rax\n"));
            write_wrapper(write!(file, "setne %al\n"));

            write_wrapper(write!(file, "{}:\n", label_b));
            
        },
        Token::TEq => {
            write_relational_partial(left, op, right, file, stack_ind,
            var_map, label_counter);
            write_wrapper(write!(file, "sete %al\n"));
        },
        Token::TNeq => {
            write_relational_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setne %al\n"));
        },
        Token::TLess => {
            write_relational_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setl %al\n"));
        },
        Token::TLeq => {
            write_relational_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setle %al\n"));
        },
        Token::TGreater => {
            write_relational_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setge %al\n"));
        },
        Token::TGeq => {
            write_relational_partial(left, op, right, file, stack_ind,
                var_map, label_counter);
             write_wrapper(write!(file, "setg %al\n"));
        },

        _ => panic!("Invalid operator found in binaryOp code gen"),
    }
    

}

fn process_return(tree: ASTTree, mut file: &File, stack_ind: i32, 
    var_map: HashMap<String, i32>, label_counter: &mut i32) {
    process_expression(tree, file, stack_ind, var_map, label_counter);
    write_epilogue(file);
    write_wrapper(write!(file, "{}", "retq\n"));
}

fn process_declare(tree: ASTTree, mut file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
    -> (i32, HashMap<String, i32>) {
    
    match tree {
        ASTTree::Declare(id, child_opt) => {
            if var_map.get(&id) != None {
                panic!("Attempted to declare variable of same name twice");
            };

            match child_opt {
                // Declared with no initialization
                None => {
                    write_wrapper(write!(file, "movq $0, %rax\n"));
                    write_wrapper(write!(file, "push %rax\n"));
                },

                // Declared with initialization
                Some(inner_child) => {
                    process_expression(*inner_child, file, stack_ind, 
                        var_map.clone(), label_counter);
                    write_wrapper(write!(file, "push %rax\n"));
                },
            }

            var_map.insert(id, stack_ind);
            stack_ind = stack_ind - 8;
        },
        _ => panic!("process_declare called on non declare statement"),
    };

    return (stack_ind, var_map);
}