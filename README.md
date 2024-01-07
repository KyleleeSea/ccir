<br />
<div align="center">
<h1 align="center">CCIR</h1>

  <p align="center">
    <h4>
      A C Compiler In Rust
    </h4>
    <br />
  </p>
</div>

<!-- ABOUT THE PROJECT -->
## About The Project
A Compiler translating C code (coherent to the C11 standard) into x86-64 bit assembly. Follows the cdecl calling convention. Grammar documentation follows Backus–Naur form.

### Supported Features
- Unary Operators
    - Arithmetic Negation (-)
    - Logical Negation (!)
    - Bitwise Complement (~)
- Arithmetic Binary Operators
    - Addition (+)
    - Subtraction (-)
    - Multiplication (*)
    - Division (/)
    - Modulo (%)
    - Logical And (&&)
    - Logical Or (||)
    - Bitwise And (&)
    - Bitwise Or (|)
    - Left Shift (<<)
    - Right Shift (>>)
    - Xor (^)
- Relational Binary Operators
    - Equality (==)
    - Inequality (!=)
    - Comparison (<, >, <=, >=)
- Local Variables
    - Assignment
    - Declaration
- Global Variables
    - Implementation consistent with position-independent execution
    - Calculates constant arithmetic expressions at compile time
- Conditionals
    - Ternary Expressions
    - If Statements
    - If-Else Statements
- Compound Statements
- Variable Scoping
- Variable Shadowing
- Loops
    - While
    - Do-While
    - For
- Functions and Function Calls
- Validation to reject programs inconsistent with the C11 standard for all features listed above

### Planned Features
- Support for types beyond ints
- Pointers
- Additional optimization layers

### Architecture
CCIR follows a three step architecture:
- Lexing: Reads a .c file and converts the contents into a token stack
- Parsing: Executes semantic analysis to convert the token stack into an abstract syntax tree
- Code Generation: Traverses AST to produce x86 assembly 

### Known Issues
- MacOS does not natively support compilation of x86 assembly. If you wish to convert the generated .s file into an executable, please install Rosetta. See <a href="https://stackoverflow.com/questions/64882584/how-to-run-the-homebrew-installer-under-rosetta-2-on-m1-macbook">here</a>.
- Requires int functions have a return statement, inconsistent with C11's specification that int functions without a return statement should automatically return 0.
- r10 and r11 not caller saved
- rbx not callee saved

## Local Copy
You may run this compiler locally by following these steps:

1. Clone the repo
   ```sh
   git clone https://github.com/KyleleeSea/ccir
   ```
2. ```sh
   cargo build
   ```
3. Assemble and produce executable for desired .c file
   ```sh
   cargo run (path)
   ```
4. Run the executable
   ```sh
   ./out
   ```

The above assumes the following:
1. Installation of rust. If you do not have rust, please see: <a href="https://doc.rust-lang.org/book/ch01-01-installation.html">Rust installation guide</a>.
2. Installation of gcc to convert assembly into an executable.
   
## External Sources
- <a href="https://norasandler.com/archive/">Nora Sandler</a>
- <a href="https://aaronbloomfield.github.io/pdr/book/x86-64bit-ccc-chapter.pdf">Adam Ferrari</a>