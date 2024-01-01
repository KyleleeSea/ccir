.globl _main
_main:
push %rbp
movq %rsp, %rbp
movq $1, %rax
push %rax
movq $2, %rax
pop %rcx
addq %rcx, %rax
