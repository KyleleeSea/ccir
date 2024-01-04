use std::fs::OpenOptions;
use std::fs::File;
use std::io::Write;
use super::types::ASTTree;
use super::types::Token;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::collections::HashMap;
use std::collections::HashSet;

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

            let stack_ind : i32 = -8;
            let var_map = HashMap::new();

            process_compound(children, &file, stack_ind, var_map,
                label_counter);

        },
        _ => invalid_match("process_function"),
    }
}

// Process_block responsible for variable scoping
fn process_block(tree: ASTTree, file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32,
    mut curr_scope : HashSet<String>)
    -> (i32, HashMap<String, i32>, HashSet<String>) {

    match tree {
        ASTTree::BlockItem(child) => match *child {
            ASTTree::Declare(ref _id, ref _inner_child) => 
            (stack_ind, var_map, curr_scope) = process_declare(*child, file, 
                stack_ind, var_map, label_counter, curr_scope),

            _ => {
                process_statement(*child, file, stack_ind, var_map.clone(), 
            label_counter);
            // unit required to make match arms have same type
            ();
        },
        },
        _ => panic!("process_block failed"),
    };

    return (stack_ind, var_map, curr_scope);
}

// Statement can be Return, Declare, Compound, NullExp,
// For, ForDecl, While, Do, Continue, Break, or an arbitrary expression
fn process_statement(tree: ASTTree, file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
    -> (i32, HashMap<String, i32>) {
    match tree {
        ASTTree::Statement(child) => match *child {
            ASTTree::Return(inner_child) => process_return(*inner_child, file, 
                stack_ind, var_map.clone(), label_counter),

            ASTTree::Conditional(ref _condition, ref _s1, ref _s2) => 
            process_conditional(*child, file, stack_ind, 
                var_map.clone(), label_counter),

            ASTTree::Compound(block_list) => (stack_ind, var_map) = 
                process_compound(block_list, file, stack_ind, var_map, 
                    label_counter),

            ASTTree::NullExp => (),

            ASTTree::Continue => print!("continue not implemented\n"),

            ASTTree::Break => print!("break not implemented\n"),

            // might need to set stack_ind and var_map in fors, havent thought abt it 
            // might not need to though idk 
            ASTTree::For(initopt, control, postopt, body) => process_for(initopt, 
                control, postopt, body, file, stack_ind, var_map.clone(), 
                label_counter),

            ASTTree::ForDecl(decl, control, postopt, body) => process_for_decl(decl, 
                control, postopt, body, file, stack_ind, var_map.clone(), 
                label_counter),

            ASTTree::While(condition, body) => process_while(condition, body, 
                file, stack_ind, var_map.clone(), label_counter),

            ASTTree::Do(body, condition) => process_do(body, condition,
                file, stack_ind, var_map.clone(), label_counter),

            _ => process_expression(*child, file, stack_ind, var_map.clone(), 
            label_counter),
        },
        _ => panic!("failed process_statement"),
    };

    return (stack_ind, var_map);
}

fn process_for(initopt: Option<Box<ASTTree>>, control: Box<ASTTree>,
    postopt: Option<Box<ASTTree>>, body: Box<ASTTree>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {
    
    print!("for not yet implemented\n");

}

fn process_for_decl(decl: Box<ASTTree>, control: Box<ASTTree>,
    postopt: Option<Box<ASTTree>>, body: Box<ASTTree>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {
        
    print!("for decl not yet implemented\n");

}

fn process_while(condition: Box<ASTTree>, body: Box<ASTTree>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {
    
    let guard_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);

    write_wrapper(write!(file, "{}:\n", guard_label));
    // Evaluate expression
    process_expression(*condition, file, stack_ind, var_map.clone(),
    label_counter);
    // If it is false, exit the loop
    write_wrapper(write!(file, "cmpq $0, %rax\n"));
    write_wrapper(write!(file, "je {}\n", end_of_loop_label));
    // Execute statement
    process_statement(*body, file, stack_ind, var_map.clone(), label_counter);
    // Jump to the guard label
    write_wrapper(write!(file, "jmp {}\n", guard_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));
}

fn process_do(body: Box<ASTTree>, condition: Box<ASTTree>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {
    
    let start_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);

    write_wrapper(write!(file, "{}:\n", start_label));
    // 1. Execute statement
    process_statement(*body, file, stack_ind, var_map.clone(), label_counter);
    // 2. Evaluate expression
    process_expression(*condition, file, stack_ind, var_map.clone(),
    label_counter);
    // 3. If it is false, exit
    write_wrapper(write!(file, "cmpq $0, %rax\n"));
    write_wrapper(write!(file, "je {}\n", end_of_loop_label));
    // 4. Jump to step 1
    write_wrapper(write!(file, "jmp {}\n", start_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));

}


fn process_compound(block_list: Vec<Box<ASTTree>>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, i32>, 
    label_counter: &mut i32) -> (i32, HashMap<String, i32>) {

    let mut curr_scope = HashSet::new();

    for block in block_list {
        (stack_ind, var_map, curr_scope) = process_block(*block, &file, 
            stack_ind, var_map, label_counter, curr_scope);
    }

    // deallocate vars
    let bytes_to_dealloc = 8 * curr_scope.len();
    write_wrapper(write!(file, "addq ${}, %rsp\n", bytes_to_dealloc));

    return (stack_ind, var_map)
}

fn process_conditional(tree: ASTTree, mut file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, i32>, label_counter: &mut i32)
{
    match tree {
        ASTTree::Conditional(condition, s1, s2_opt) => {
            match s2_opt {
                None => {
                    let label_a = get_unique_label(label_counter);

                    process_expression(*condition, file, stack_ind, 
                        var_map.clone(), label_counter);
                    write_wrapper(write!(file, "cmpq $0, %rax\n"));
                    write_wrapper(write!(file, "je {}\n", label_a));

                    process_statement(*s1, file, 
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

                    process_statement(*s1, file, 
                        stack_ind, var_map.clone(), label_counter);
                    write_wrapper(write!(file, "jmp {}\n", label_b));

                    write_wrapper(write!(file, "{}:\n", label_a));
                    process_statement(*s2, file, 
                        stack_ind, var_map, label_counter);
                    write_wrapper(write!(file, "{}:\n", label_b));

                },
            }
        },
        _ => invalid_match("process_conditional"),
    };

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
        },

        ASTTree::CondExp(e1, e2, e3) => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);

            process_expression(*e1, file, stack_ind, var_map.clone(), label_counter);

            write_wrapper(write!(file, "cmpq $0, %rax\n"));
            write_wrapper(write!(file, "je {}\n", label_a));

            process_expression(*e2, file, stack_ind, var_map.clone(), label_counter);

            write_wrapper(write!(file, "jmp {}\n", label_b));

            write_wrapper(write!(file, "{}:\n", label_a));

            process_expression(*e3, file, stack_ind, var_map.clone(), label_counter);

            write_wrapper(write!(file, "{}:\n", label_b));
        }

        _ => invalid_match("process exp"),
    }
}

fn write_arithmetic_partial(left: ASTTree, right: ASTTree,  
    mut file: &File, stack_ind: i32,  var_map: HashMap<String, i32>, 
    label_counter: &mut i32) {

    process_expression(left, file, stack_ind, var_map.clone(), label_counter);
    write_wrapper(write!(file, "push %rax\n"));
    process_expression(right, file, stack_ind, var_map, label_counter);
    // e1 in rcx, e2 in rax
    write_wrapper(write!(file, "pop %rcx\n"));
}

fn write_relational_partial(left: ASTTree, right: ASTTree,  
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
            write_arithmetic_partial(left, right, file, stack_ind,
            var_map, label_counter);
            write_wrapper(write!(file, "addq %rcx, %rax\n"));
        },
        
        Token::TMultiply => {
            write_arithmetic_partial(left, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "imul %rcx, %rax\n"));
        },

        Token::TBitAnd => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "and %rcx, %rax\n"));
        }

        Token::TBitOr => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "or %rcx, %rax\n"));
        }

        Token::TXor => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "xorq %rcx, %rax\n"));
        }

        Token::TRShift => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "push %rax\n"));
            write_wrapper(write!(file, "movq %rcx, %rax\n"));
            write_wrapper(write!(file, "pop %rcx\n"));
            write_wrapper(write!(file, "sarq %cl, %rax\n"));
        }

        Token::TLShift => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter);
            write_wrapper(write!(file, "push %rax\n"));
            write_wrapper(write!(file, "movq %rcx, %rax\n"));
            write_wrapper(write!(file, "pop %rcx\n"));
            write_wrapper(write!(file, "shl %cl, %rax\n"));
        }

        Token::TNeg => {
            write_arithmetic_partial(left, right, file, stack_ind,
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
            write_arithmetic_partial(left, right, file, stack_ind,
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
            write_arithmetic_partial(left, right, file, stack_ind,
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
            write_relational_partial(left, right, file, stack_ind,
            var_map, label_counter);
            write_wrapper(write!(file, "sete %al\n"));
        },
        Token::TNeq => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setne %al\n"));
        },
        Token::TLess => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setl %al\n"));
        },
        Token::TLeq => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setle %al\n"));
        },
        Token::TGreater => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter);
            write_wrapper(write!(file, "setge %al\n"));
        },
        Token::TGeq => {
            write_relational_partial(left, right, file, stack_ind,
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
    mut var_map: HashMap<String, i32>, label_counter: &mut i32, 
    mut curr_scope : HashSet<String>)
    -> (i32, HashMap<String, i32>, HashSet<String>) {
    
    match tree {
        ASTTree::Declare(id, child_opt) => {
            if curr_scope.get(&id) != None {
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

            var_map.insert(id.clone(), stack_ind);
            stack_ind = stack_ind - 8;
            curr_scope.insert(id);
        },
        _ => panic!("process_declare called on non declare statement"),
    };

    return (stack_ind, var_map, curr_scope);
}