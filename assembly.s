.globl _main
_main:
push %rbp
movq %rsp, %rbp
movq $2, %rax
push %rax
movq -8(%rbp), %rax
movq %rbp, %rsp
pop %rbp
retq
