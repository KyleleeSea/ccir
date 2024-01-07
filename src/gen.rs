use std::fs::OpenOptions;
use std::fs::File;
use std::io::Write;
use super::types::ASTTree;
use super::types::Token;
use super::types::VarType;
use super::types::FnType;
use super::types::GlobalType;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::collections::HashMap;
use std::collections::HashSet;

fn write_prologue(mut file: &File) {
    write_wrapper(write!(file, "\tpush \t%rbp\n"));
    write_wrapper(write!(file, "\tmovq \t%rsp, %rbp\n"));
    write_wrapper(write!(file, "\tpush \t%r12\n"));
    write_wrapper(write!(file, "\tpush \t%r13\n"));
    write_wrapper(write!(file, "\tpush \t%r14\n"));
    write_wrapper(write!(file, "\tpush \t%r15\n"));
    // rbx also callee saved, not saving because we don't use rbx anywhere
}

fn write_epilogue(mut file: &File) {
    write_wrapper(write!(file, "\tpop \t%r15\n"));
    write_wrapper(write!(file, "\tpop \t%r14\n"));
    write_wrapper(write!(file, "\tpop \t%r13\n"));
    write_wrapper(write!(file, "\tpop \t%r12\n"));
    write_wrapper(write!(file, "\tmovq \t%rbp, %rsp\n"));
    write_wrapper(write!(file, "\tpop \t%rbp\n"));
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

// precondition 0 <= ind <= 5
fn ind_to_arg_register(ind: usize) -> String {
    match ind {
        0 => String::from("rdi"),
        1 => String::from("rsi"),
        2 => String::from("rdx"),
        3 => String::from("rcx"),
        4 => String::from("r8"),
        5 => String::from("r9"),
        _ => panic!("ind argument called outside of range"),
    }
}

pub fn generate(tree: ASTTree) {
    if Path::new("assembly.s").exists() {
      fs::remove_file("assembly.s").unwrap();
    }

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open("assembly.s")
        .unwrap();

    let mut label_counter : i32 = 0;
    let mut fn_validator : HashMap<String, FnType> = HashMap::new();    
    let mut globals_validator : HashMap<String, GlobalType> = HashMap::new();
    let mut fn_names : HashSet<String> = HashSet::new();
    let mut global_names : HashSet<String> = HashSet::new();    
    // 5 = number of args we push in prologue
    let stack_ind : i32 = -8 * 5;
    let mut var_map = HashMap::new();
    
    match tree {
        ASTTree::Program(children) => {
            for child in children {
                match *child {
                    ASTTree::Function(id, args, bodyopt) => {
                        let num_args_curr = args.len();
                        // Validation start
                        match fn_validator.get(&id) {
                            None => {
                                match bodyopt {
                                    None => fn_validator.insert(id.clone(), 
                                        FnType::Decl(num_args_curr)),
                                    Some(ref _body) => fn_validator.insert(id.clone(), 
                                        FnType::Defn(num_args_curr)),
                                };
                            },
                            Some(&FnType::Defn(num_args)) => {
                                if num_args_curr != num_args {
                                    panic!("same function declared multiple times
                                    with different parameters");
                                };

                                match bodyopt {
                                    Some(ref _body) => panic!("Cannot define same
                                        function multiple times"),
                                    None => (),
                                };

                            },
                            Some(&FnType::Decl(num_args)) => {
                                if num_args_curr != num_args {
                                    panic!("same function declared multiple times
                                    with different parameters");
                                };
                                match bodyopt {
                                    Some(ref _body) => {
                                        fn_validator.insert(id.clone(), 
                                        FnType::Defn(num_args_curr));
                                        ();
                                    },
                                    None => (),
                                };

                            },
                        }

                        match global_names.get(&id) {
                            Some(&ref _name) => panic!("reuse of label for both
                            function and global variable not allowed"),
                            None => ()
                        };

                        fn_names.insert(id.clone());

                        // Validation end

                        process_function(id, args, bodyopt, &file, stack_ind,
                            var_map.clone(), &mut label_counter, &fn_validator);
                        },

                    ASTTree::Declare(id, inner_child) => {
                        match fn_names.get(&id) {
                            Some(&ref _name) => panic!("reuse of label for both
                            function and global variable not allowed"),
                            None => ()
                        };

                        global_names.insert(id.clone());

                        (var_map, globals_validator) = process_global_declare(id, 
                            inner_child, &file, var_map, globals_validator);
                    },
                    _ => invalid_match("program")
                }
            }
        },
        _ => invalid_match("generate"),
    }

    // Post-processing, put all uninitialized globals in common
    for (id, state) in globals_validator {
        if state == GlobalType::Decl {
            write_wrapper(write!(file, "\t.comm _{},4,2\n",id));
        }
    }

    Command::new("gcc")
    .arg("assembly.s")
    .arg("-o")
    .arg("out")
    .output()
    .expect("Failed to execute command");
}

// Callee
fn process_function(id: String, args: Vec<String>, bodyopt: Option<Box<ASTTree>>, 
    mut file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, VarType>, label_counter: &mut i32, 
    fn_validator: &HashMap<String, FnType>) {

    match bodyopt {
        // Case 1) Function declaration, no assembly needed
        None => (),
        // Case 2) Function definition
        Some(body) => {
            write_wrapper(write!(file, "\t.globl _{}\n", id));
            write_wrapper(write!(file, "_{}:\n", id));
            // write_prologue takes care of callee saving
            write_prologue(&file);

            // +8 ebp pushed, +8 return addr pushed because of call
            let mut param_offset = 16;
            let mut curr_scope = HashSet::new();

            // account for stack space taken up by caller register saving
            if id != String::from("main") {
                // 6 caller registers saved
                stack_ind = stack_ind - (8*6);
            }

            // add arguments to var_map
            for (ind, elt) in args.iter().enumerate() {
                if ind <= 5 {
                    var_map.insert(elt.to_string(), VarType::Reg(String::from(
                        ind_to_arg_register(ind))));
                    curr_scope.insert(elt.to_string());
                }
                else {
                    var_map.insert(elt.to_string(), VarType::Stk(param_offset));
                    param_offset = param_offset+8;
                    curr_scope.insert(elt.to_string());
                }
            }

            match *body {
                ASTTree::Compound(children) => process_compound(children, &file, 
                    stack_ind, var_map, label_counter, None, None, fn_validator,
                    Some(curr_scope)),
                _ => panic!("compound not found as body in process_function"),
            };  
        },
    }
}

// Caller
fn process_func_call(id: String, mut args: Vec<Box<ASTTree>>, mut file: &File, 
    mut stack_ind: i32, var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {
    // validate consistent number of arguments
    match fn_validator.get(&id) {
        None => panic!("function {} referenced before declaration", id),
        Some(&FnType::Defn(num_args)) => {
            if args.len() != num_args {
                panic!("inconsistent number of functions args for {}", id);
            };
        },
        Some(&FnType::Decl(num_args)) => {
            if args.len() != num_args {
                panic!("inconsistent number of functions args for {}", id);
            };
        },
    }

    // save caller registers, could optimize by only saving 
    // the ones you use. also r10 and r11 caller saved
    let num_saved_u : usize = 6;
    let num_saved_i : i32 = 6;

    write_wrapper(write!(file, "\tpush \t%rdi\n"));
    write_wrapper(write!(file, "\tpush \t%rsi\n"));
    write_wrapper(write!(file, "\tpush \t%rdx\n"));
    write_wrapper(write!(file, "\tpush \t%rcx\n"));
    write_wrapper(write!(file, "\tpush \t%r8\n"));
    write_wrapper(write!(file, "\tpush \t%r9\n"));

   // Calculate padding to maintain 16 byte alignment on call
   write_wrapper(write!(file, "\tmovq \t%rsp, %rax\n"));
   if args.len() > num_saved_u {
       // first 6 args already accounted for by automatic push of caller 
       // saved registers
       write_wrapper(write!(file, "\tsubq \t${}, %rax\n", 8*((args.len() - 
       num_saved_u) + 1)));
   }

   // zero out rdx which will contain division remainder
   write_wrapper(write!(file, "\txorq \t%rdx, %rdx\n"));
   // 0x20 = 16 in decimal
   write_wrapper(write!(file, "\tmovq \t$0x20, %rcx\n"));
   // edx will contain the remainder
   write_wrapper(write!(file, "\tidivq \t%rcx\n"));
   // pad rsp
   write_wrapper(write!(file, "\tsubq \t%rdx, %rsp\n"));
   write_wrapper(write!(file, "\tpushq \t%rdx\n"));

    // +1 is the padding push
    stack_ind = stack_ind - (8 * (num_saved_i+1));

    let mut ind = 0;
    // Case 1) All arguments go into registers
    if args.len() <= num_saved_u {
        for arg in args {
            process_expression(*arg, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator);

            write_wrapper(write!(file, "\tmovq \t%rax, %{}\n", 
                ind_to_arg_register(ind)));

            ind = ind + 1;
        }   

        write_wrapper(write!(file, "\tcall _{}\n", id));

    }
    // Case 2) Some arguments must go on the stack
    else {
        let mut stk_args = args.split_off(num_saved_u);
        stk_args.reverse();

        for arg in args {
            process_expression(*arg, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator);

            write_wrapper(write!(file, "\tmovq \t%rax, %{}\n", 
                ind_to_arg_register(ind)));
            
            ind = ind + 1;
        }

        let stack_arg_bytes = 8 * stk_args.len();
        for arg in stk_args {
            process_expression(*arg, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator);
            write_wrapper(write!(file, "\tpushq \t%rax\n"));
            stack_ind = stack_ind - 8;
        }

        write_wrapper(write!(file, "\tcall _{}\n", id));

        // Dealloc stack
        if stack_arg_bytes > 0 {
            write_wrapper(write!(file, "\taddq \t${}, %rsp\n", stack_arg_bytes));
        }
    }

    // pop padding
    write_wrapper(write!(file, "\tpopq \t%rdx\n"));
    // remove padding from rsp
    write_wrapper(write!(file, "\taddq \t%rdx, %rsp\n"));

    // restore caller registers
    write_wrapper(write!(file, "\tpop \t%r9\n"));
    write_wrapper(write!(file, "\tpop \t%r8\n"));
    write_wrapper(write!(file, "\tpop \t%rcx\n"));
    write_wrapper(write!(file, "\tpop \t%rdx\n"));
    write_wrapper(write!(file, "\tpop \t%rsi\n"));
    write_wrapper(write!(file, "\tpop \t%rdi\n"));
}

// Process_block responsible for variable scoping
fn process_block(tree: ASTTree, file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, VarType>, label_counter: &mut i32,
    mut curr_scope : HashSet<String>, c_label_opt: Option<String>, 
    b_label_opt: Option<String>, fn_validator: &HashMap<String, FnType>)
    -> (i32, HashMap<String, VarType>, HashSet<String>) {

    match tree {
        ASTTree::BlockItem(child) => match *child {
            ASTTree::Declare(ref _id, ref _inner_child) => 
            (stack_ind, var_map, curr_scope) = process_declare(*child, file, 
                stack_ind, var_map, label_counter, curr_scope, fn_validator),

            _ => {
                process_statement(*child, file, stack_ind, var_map.clone(), 
            label_counter, c_label_opt, b_label_opt, fn_validator);
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
    mut var_map: HashMap<String, VarType>, label_counter: &mut i32,
    c_label_opt: Option<String>, b_label_opt: Option<String>, 
    fn_validator: &HashMap<String, FnType>)
    -> (i32, HashMap<String, VarType>) {
    match tree {
        ASTTree::Statement(child) => match *child {
            ASTTree::Return(inner_child) => process_return(*inner_child, file, 
                stack_ind, var_map.clone(), label_counter, fn_validator),

            ASTTree::Conditional(ref _condition, ref _s1, ref _s2) => 
            process_conditional(*child, file, stack_ind, 
                var_map.clone(), label_counter, c_label_opt, b_label_opt, 
                fn_validator),

            ASTTree::Compound(block_list) => (stack_ind, var_map) = 
                process_compound(block_list, file, stack_ind, var_map, 
                    label_counter, c_label_opt, b_label_opt, fn_validator, None),

            ASTTree::NullExp => (),

            ASTTree::Continue => process_continue(c_label_opt, file),

            ASTTree::Break => process_break(b_label_opt, file),

            // might need to set stack_ind and var_map in fors, havent thought abt it 
            // might not need to though idk 
            ASTTree::For(initopt, control, postopt, body) => process_for(initopt, 
                control, postopt, body, file, stack_ind, var_map.clone(), 
                label_counter, fn_validator),

            ASTTree::ForDecl(decl, control, postopt, body) => process_for_decl(decl, 
                control, postopt, body, file, stack_ind, var_map.clone(), 
                label_counter, fn_validator),

            ASTTree::While(condition, body) => process_while(condition, body, 
                file, stack_ind, var_map.clone(), label_counter, fn_validator),

            ASTTree::Do(body, condition) => process_do(body, condition,
                file, stack_ind, var_map.clone(), label_counter, fn_validator),

            ASTTree::FuncCall(id, args) => process_func_call(id, args, file,
            stack_ind, var_map.clone(), label_counter, fn_validator),

            _ => process_expression(*child, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator),
        },
        _ => panic!("failed process_statement"),
    };

    return (stack_ind, var_map);
}

fn process_continue(c_label_opt: Option<String>, mut file: &File) {
    match c_label_opt {
        None => panic!("continue called outside of a loop!"),
        Some(label) => write_wrapper(write!(file, "\tjmp \t{}\n", label)),
    };
}

fn process_break(b_label_opt: Option<String>, mut file: &File) {
    match b_label_opt {
        None => panic!("break called outside of a loop!"),
        Some(label) => write_wrapper(write!(file, "\tjmp \t{}\n", label)),
    };
}

fn process_for(initopt: Option<Box<ASTTree>>, control: Box<ASTTree>,
    postopt: Option<Box<ASTTree>>, body: Box<ASTTree>, mut file: &File, 
    stack_ind: i32, var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {

    let guard_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);
    // exists for continue, as post-exp should still be executed
    let end_of_body_label = get_unique_label(label_counter);

    // 1. Evaluate init
    match initopt {
        None => (),
        Some(init) => process_expression(*init, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator),
    };

    write_wrapper(write!(file, "{}:\n", guard_label));
    // 2. Evaluate condition
    process_expression(*control, file, stack_ind, var_map.clone(), 
        label_counter, fn_validator);
    // 3. If condition false, then exit
    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
    write_wrapper(write!(file, "\tje \t{}\n", end_of_loop_label));

    // 4. Execute statement
    process_statement(*body, file, stack_ind, 
        var_map.clone(), label_counter, Some(end_of_body_label.clone()), 
        Some(end_of_loop_label.clone()), fn_validator);

    write_wrapper(write!(file, "{}:\n", end_of_body_label));
    // 5. Execute post-expression
    match postopt {
        None => (),
        Some(post_op) => process_expression(*post_op, file, stack_ind, 
            var_map.clone(), label_counter, fn_validator),
    };

    // 6. Jump to step 2
    write_wrapper(write!(file, "\tjmp \t{}\n", guard_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));
}

fn process_for_decl(decl: Box<ASTTree>, control: Box<ASTTree>,
    postopt: Option<Box<ASTTree>>, body: Box<ASTTree>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {
        
    let guard_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);
    let end_of_body_label = get_unique_label(label_counter);
    let mut curr_scope = HashSet::new();

    // 1. Evaluate decl
    (stack_ind, var_map, curr_scope) = process_declare(*decl, file, stack_ind, 
        var_map.clone(), label_counter, curr_scope.clone(), fn_validator);

    write_wrapper(write!(file, "{}:\n", guard_label));
    // 2. Evaluate condition
    process_expression(*control, file, stack_ind, var_map.clone(), 
        label_counter, fn_validator);
    // 3. If condition false, then exit
    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
    write_wrapper(write!(file, "\tje {}\n", end_of_loop_label));

    // 4. Execute statement
    process_statement(*body, file, stack_ind, 
        var_map.clone(), label_counter, Some(end_of_body_label.clone()), 
        Some(end_of_loop_label.clone()), fn_validator);

    write_wrapper(write!(file, "{}:\n", end_of_body_label));
    // 5. Execute post-expression
    match postopt {
        None => (),
        Some(post_op) => process_expression(*post_op, file, stack_ind, 
            var_map.clone(), label_counter, fn_validator),
    };

    // 6. Jump to step 2
    write_wrapper(write!(file, "\tjmp {}\n", guard_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));
    // 7. Deallocate variable declared in for loop header
    let bytes_to_dealloc = 8 * curr_scope.len();
    write_wrapper(write!(file, "\taddq \t${}, %rsp\n", bytes_to_dealloc));


}

fn process_while(condition: Box<ASTTree>, body: Box<ASTTree>, mut file: &File, 
    stack_ind: i32, var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {
    
    let guard_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);

    write_wrapper(write!(file, "{}:\n", guard_label));
    // Evaluate expression
    process_expression(*condition, file, stack_ind, var_map.clone(),
    label_counter, fn_validator);
    // If it is false, exit the loop
    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
    write_wrapper(write!(file, "\tje \t{}\n", end_of_loop_label));
    // Execute statement
    process_statement(*body, file, stack_ind,
        var_map.clone(), label_counter,
    Some(guard_label.clone()), Some(end_of_loop_label.clone()), fn_validator);
    // Jump to the guard label
    write_wrapper(write!(file, "\tjmp \t{}\n", guard_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));
}

fn process_do(body: Box<ASTTree>, condition: Box<ASTTree>, mut file: &File, 
    stack_ind: i32, var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {
    
    let start_label = get_unique_label(label_counter);
    let end_of_loop_label = get_unique_label(label_counter);

    write_wrapper(write!(file, "{}:\n", start_label));
    // 1. Execute statement
    process_statement(*body, file, stack_ind, 
        var_map.clone(), label_counter,
    Some(start_label.clone()), Some(end_of_loop_label.clone()), fn_validator);
    // 2. Evaluate expression
    process_expression(*condition, file, stack_ind, var_map.clone(),
    label_counter, fn_validator);
    // 3. If it is false, exit
    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
    write_wrapper(write!(file, "\tje \t{}\n", end_of_loop_label));
    // 4. Jump to step 1
    write_wrapper(write!(file, "\tjmp \t{}\n", start_label));

    write_wrapper(write!(file, "{}:\n", end_of_loop_label));
}


fn process_compound(block_list: Vec<Box<ASTTree>>, mut file: &File, 
    mut stack_ind: i32, mut var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, c_label_opt: Option<String>, 
    b_label_opt: Option<String>, fn_validator: &HashMap<String, FnType>,
    curr_scope_opt: Option<HashSet<String>>) 
    -> (i32, HashMap<String, VarType>) {

    let mut curr_scope;

    // Since function arguments count as a declaration in the body compound
    match curr_scope_opt {
        None => curr_scope = HashSet::new(),
        Some(inner) => curr_scope = inner,
    };

    for block in block_list {
        (stack_ind, var_map, curr_scope) = process_block(*block, &file, 
            stack_ind, var_map, label_counter, curr_scope,
        c_label_opt.clone(), b_label_opt.clone(), fn_validator);
    }

    // deallocate vars
    let bytes_to_dealloc = 8 * curr_scope.len();
    write_wrapper(write!(file, "\taddq \t${}, %rsp\n", bytes_to_dealloc));

    return (stack_ind, var_map)
}

fn process_conditional(tree: ASTTree, mut file: &File, stack_ind: i32, 
    var_map: HashMap<String, VarType>, label_counter: &mut i32,
    c_label_opt: Option<String>, b_label_opt: Option<String>, 
    fn_validator: &HashMap<String, FnType>)
{
    match tree {
        ASTTree::Conditional(condition, s1, s2_opt) => {
            match s2_opt {
                None => {
                    let label_a = get_unique_label(label_counter);

                    process_expression(*condition, file, stack_ind, 
                        var_map.clone(), label_counter, fn_validator);
                    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
                    write_wrapper(write!(file, "\tje \t{}\n", label_a));

                    process_statement(*s1, file, 
                        stack_ind, var_map, label_counter, c_label_opt,
                    b_label_opt, fn_validator);
                    
                    write_wrapper(write!(file, "{}:\n", label_a));

                },

                Some(s2) => {
                    let label_a = get_unique_label(label_counter);
                    let label_b = get_unique_label(label_counter);

                    process_expression(*condition, file, stack_ind, 
                        var_map.clone(), label_counter, fn_validator);

                    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
                    write_wrapper(write!(file, "\tje \t{}\n", label_a));

                    process_statement(*s1, file, 
                        stack_ind, var_map.clone(), label_counter,
                    c_label_opt.clone(), b_label_opt.clone(), fn_validator);
                    write_wrapper(write!(file, "\tjmp \t{}\n", label_b));

                    write_wrapper(write!(file, "{}:\n", label_a));
                    process_statement(*s2, file, 
                        stack_ind, var_map, label_counter, c_label_opt.clone(),
                        b_label_opt.clone(), fn_validator);
                    write_wrapper(write!(file, "{}:\n", label_b));

                },
            }
        },
        _ => invalid_match("process_conditional"),
    };

}

fn process_expression(tree: ASTTree, mut file: &File, stack_ind: i32, 
    var_map: HashMap<String, VarType>, label_counter: &mut i32, 
    fn_validator: &HashMap<String, FnType>)
    {
    match tree {
        ASTTree::Constant(x) => {
            write_wrapper(write!(file, "\tmovq \t${}, %rax\n", x));
        },
        ASTTree::UnaryOp(op, child) => {
            process_expression(*child, file, stack_ind, 
                var_map, label_counter, fn_validator);
            match op {
                Token::TNeg => write_wrapper(write!(file, "\tneg \t%rax\n")),
                Token::TBitComp => write_wrapper(write!(file, "\tnot \t%rax\n")),
                Token::TLNeg => {
                    write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
                    write_wrapper(write!(file, "\tmovq \t$0, %rax\n"));
                    write_wrapper(write!(file, "\tsete \t%al\n"));
                },
                _ => panic!("Invalid operator found in unaryOp code gen"),
            }
        },
        ASTTree::BinaryOp(left, op, right) => 
        process_binary_op(*left, op, *right, file, 
            stack_ind, var_map, label_counter, fn_validator),

        ASTTree::Var(id) => {
            match var_map.get(&id) {
                None => panic!("Var referenced without declaration"),
                Some(&ref inner) => 
                    match inner {
                        VarType::Reg(register) => write_wrapper(write!(file, 
                            "\tmovq \t%{}, %rax\n", register)),
                        VarType::Stk(offset) => write_wrapper(write!(file, 
                            "\tmovq \t{}(%rbp), %rax\n", offset)),
                        VarType::Global(id) => write_wrapper(write!(file,
                        "\tmovq \t_{}(%rip), %rax\n", id)),
                    },
            }
        },

        ASTTree::Assign(id, child) => {
            process_expression(*child, file, stack_ind, 
                var_map.clone(), label_counter, fn_validator);
            match var_map.get(&id) {
                None => panic!("Var assignment without declaration"),
                Some(&ref inner) => 
                    match inner {
                        VarType::Reg(register) => write_wrapper(write!(file, 
                            "\tmovq \t%rax, %{}\n", register)),
                        VarType::Stk(offset) => write_wrapper(write!(file, 
                            "\tmovq \t%rax, {}(%rbp)\n", offset)),
                        VarType::Global(id) => write_wrapper(write!(file,
                            "\tmovq \t%rax, _{}(%rip)\n", id)),
                    },
            }     
        },

        ASTTree::CondExp(e1, e2, e3) => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);

            process_expression(*e1, file, stack_ind, var_map.clone(), label_counter,
            fn_validator);

            write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tje \t{}\n", label_a));

            process_expression(*e2, file, stack_ind, var_map.clone(), label_counter,
            fn_validator);

            write_wrapper(write!(file, "\tjmp \t{}\n", label_b));

            write_wrapper(write!(file, "{}:\n", label_a));

            process_expression(*e3, file, stack_ind, var_map.clone(), label_counter,
            fn_validator);

            write_wrapper(write!(file, "{}:\n", label_b));
        },


        ASTTree::FuncCall(id, args) => process_func_call(id, args, file,
            stack_ind, var_map, label_counter, fn_validator),

        _ => invalid_match("process exp"),
    }
}

fn write_arithmetic_partial(left: ASTTree, right: ASTTree,  
    mut file: &File, stack_ind: i32,  var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {

    process_expression(left, file, stack_ind, var_map.clone(), label_counter,
    fn_validator);
    write_wrapper(write!(file, "\tpush \t%rax\n"));
    process_expression(right, file, stack_ind, var_map, label_counter,
        fn_validator);
    // e1 in r13, e2 in rax
    write_wrapper(write!(file, "\tpop \t%r13\n"));
}

fn write_relational_partial(left: ASTTree, right: ASTTree,  
    mut file: &File, stack_ind: i32,  var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {

    process_expression(left, file, stack_ind, var_map.clone(), label_counter,
    fn_validator);
    write_wrapper(write!(file, "\tpush \t%rax\n"));
    process_expression(right, file, stack_ind, var_map, label_counter, 
        fn_validator);
    // e1 in r13, e2 in rax
    write_wrapper(write!(file, "\tpop \t%r13\n"));
    write_wrapper(write!(file, "\tcmpq \t%rax, %r13\n"));
    write_wrapper(write!(file, "\tmovq \t$0, %rax\n"));

}

fn process_binary_op(left: ASTTree, op: Token, right: ASTTree,  mut file: &File, 
    stack_ind: i32,  var_map: HashMap<String, VarType>, 
    label_counter: &mut i32, fn_validator: &HashMap<String, FnType>) {

    match op {
        Token::TAdd => {
            write_arithmetic_partial(left, right, file, stack_ind,
            var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\taddq \t%r13, %rax\n"));
        },
        
        Token::TMultiply => {
            write_arithmetic_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\timul \t%r13, %rax\n"));
        },

        Token::TBitAnd => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tand \t%r13, %rax\n"));
        }

        Token::TBitOr => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tor \t%r13, %rax\n"));
        }

        Token::TXor => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\txorq \t%r13, %rax\n"));
        }

        Token::TRShift => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tpush \t%rax\n"));
            write_wrapper(write!(file, "\tmovq \t%r13, %rax\n"));
            write_wrapper(write!(file, "\tpop \t%r13\n"));
            write_wrapper(write!(file, "\tsarq \t%cl, %rax\n"));
        }

        Token::TLShift => {
            write_arithmetic_partial(left, right, file, stack_ind, 
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tpush \t%rax\n"));
            write_wrapper(write!(file, "\tmovq \t%r13, %rax\n"));
            write_wrapper(write!(file, "\tpop \t%r13\n"));
            write_wrapper(write!(file, "\tshl \t%cl, %rax\n"));
        }

        Token::TNeg => {
            write_arithmetic_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            // want e1 in rax, e2 in ecx
            write_wrapper(write!(file, "\tmovq \t%r13, %r12\n"));
            // e1 in rbx, e2 in rax
            write_wrapper(write!(file, "\tmovq \t%rax, %r13\n"));
            // e1 in rbx, e2 in r13
            write_wrapper(write!(file, "\tmovq \t%r12, %rax\n"));
            // e1 in rax, e2 in r13
            write_wrapper(write!(file, "\tsubq \t%r13, %rax\n"));
        },
        Token::TDivide => {
            write_arithmetic_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            // Move e2 into r14 
            write_wrapper(write!(file, "\tmovq \t%rax, %r14\n"));
            // Move e1 to rax
            write_wrapper(write!(file, "\tmovq \t%r13, %rax\n"));
            // Sign extend into r12
            write_wrapper(write!(file, "\tcqo\n"));
            write_wrapper(write!(file, "\tidivq \t%r14\n"));
        },
        Token::TMod => {
            write_arithmetic_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            // e1 in r13, e2 in rax

            // Using divq S:
            // Unsigned divide %r12:%rax by S
            // Quotient stored in %rax
            // Remainder stored in %r12
            
            // Temporarily store %r12, %r14 on stack
            // Top of stack in order: %r12, %r14
            write_wrapper(write!(file, "\tpush \t%r14\n"));
            write_wrapper(write!(file, "\tpush \t%r12\n"));
            write_wrapper(write!(file, "\tmovq \t%rax, %r14\n"));

            // Clear out %r12, %r13 -> %rax
            write_wrapper(write!(file, "\txorq \t%r12, %r12\n"));
            write_wrapper(write!(file, "\tmovq \t%r13, %rax\n"));

            // %r12:%rax div %r14 
            write_wrapper(write!(file, "\tdivq \t%r14\n"));
            // Move remainder to %rax
            write_wrapper(write!(file, "\tmovq \t%r12, %rax\n"));

            // Restore %r12, %r14
            write_wrapper(write!(file, "\tpop \t%r12\n"));
            write_wrapper(write!(file, "\tpop \t%r14\n"));
        }
        Token::TAnd => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);
            process_expression(left, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator);
            write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tjne {}\n", label_a));
            write_wrapper(write!(file, "\tjmp {}\n", label_b));

            write_wrapper(write!(file, "{}:\n", label_a));
            process_expression(right, file, stack_ind, var_map, label_counter,
                fn_validator);
            write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tmovq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tsetne \t%al\n"));

            write_wrapper(write!(file, "{}:\n", label_b));
        },
        Token::TOr => {
            let label_a = get_unique_label(label_counter);
            let label_b = get_unique_label(label_counter);
            process_expression(left, file, stack_ind, var_map.clone(), 
            label_counter, fn_validator);
            write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tje \t{}\n", label_a));
            write_wrapper(write!(file, "\tmovq \t$1, %rax\n"));
            write_wrapper(write!(file, "\tjmp \t{}\n", label_b));
            
            write_wrapper(write!(file, "{}:\n", label_a));
            process_expression(right, file, stack_ind, var_map, label_counter,
                fn_validator);
            write_wrapper(write!(file, "\tcmpq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tmovq \t$0, %rax\n"));
            write_wrapper(write!(file, "\tsetne \t%al\n"));

            write_wrapper(write!(file, "{}:\n", label_b));
            
        },
        Token::TEq => {
            write_relational_partial(left, right, file, stack_ind,
            var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tsete \t%al\n"));
        },
        Token::TNeq => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tsetne \t%al\n"));
        },
        Token::TLess => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tsetl \t%al\n"));
        },
        Token::TLeq => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tsetle \t%al\n"));
        },
        Token::TGreater => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
            write_wrapper(write!(file, "\tsetg \t%al\n"));
        },
        Token::TGeq => {
            write_relational_partial(left, right, file, stack_ind,
                var_map, label_counter, fn_validator);
             write_wrapper(write!(file, "\tsetge \t%al\n"));
        },

        _ => panic!("Invalid operator found in binaryOp code gen"),
    }
}

fn process_return(tree: ASTTree, mut file: &File, stack_ind: i32, 
    var_map: HashMap<String, VarType>, label_counter: &mut i32, 
    fn_validator: &HashMap<String, FnType>) {
    process_expression(tree, file, stack_ind, var_map, label_counter,
        fn_validator);
    write_epilogue(file);
    write_wrapper(write!(file, "{}", "\tretq\n"));
}

fn process_declare(tree: ASTTree, mut file: &File, mut stack_ind: i32, 
    mut var_map: HashMap<String, VarType>, label_counter: &mut i32, 
    mut curr_scope : HashSet<String>, fn_validator: &HashMap<String, FnType>)
    -> (i32, HashMap<String, VarType>, HashSet<String>) {
    
    match tree {
        ASTTree::Declare(id, child_opt) => {
            if curr_scope.get(&id) != None {
                panic!("Attempted to declare variable of same name twice");
            };

            match child_opt {
                // Declared with no initialization
                None => {
                    write_wrapper(write!(file, "\tmovq \t$0, %rax\n"));
                    write_wrapper(write!(file, "\tpush \t%rax\n"));
                },

                // Declared with initialization
                Some(inner_child) => {
                    process_expression(*inner_child, file, stack_ind, 
                        var_map.clone(), label_counter, fn_validator);
                    write_wrapper(write!(file, "\tpush \t%rax\n"));
                },
            }

            var_map.insert(id.clone(), VarType::Stk(stack_ind));
            stack_ind = stack_ind - 8;
            curr_scope.insert(id);
        },
        _ => panic!("process_declare called on non declare statement"),
    };

    return (stack_ind, var_map, curr_scope);
}

fn write_global(id: String, mut file: &File, x: i64) {
    write_wrapper(write!(file, "\t.data\n"));
    write_wrapper(write!(file, "\t.globl _{}\n", id));
    write_wrapper(write!(file, "\t.p2align 2\n"));
    write_wrapper(write!(file, "_{}:\n", id));
    write_wrapper(write!(file, "\t.long {}\n", x));
    write_wrapper(write!(file, "\t.text\n"));
}

fn process_global_declare(id: String, inner_child_opt: Option<Box<ASTTree>>,
    file: &File, mut var_map: HashMap<String, VarType>, mut 
    globals_validator: HashMap<String, GlobalType>)
    -> (HashMap<String, VarType>, HashMap<String, GlobalType>) {

    match inner_child_opt {
        // Declaration
        None => {
            var_map.insert(id.clone(), VarType::Global(id.clone()));
            globals_validator.insert(id.clone(), GlobalType::Decl);
        },

        Some(inner_child) => {
            match *inner_child {
                ASTTree::Constant(x) => {
                    // Validation: Global vars cannot be defined multiple times
                    match globals_validator.get(&id) {
                        Some(&GlobalType::Defn) => panic!("cannot define
                            global variables multiple times"),
                        _ => {
                            var_map.insert(id.clone(), 
                                VarType::Global(id.clone()));
                            globals_validator.insert(id.clone(), 
                                GlobalType::Defn);
                            write_global(id, file, x);
                        },
                    };


                },
                _ => panic!("Non-constant expression found in global var defn!")
            }
            
        }
    }

    return (var_map, globals_validator)
}