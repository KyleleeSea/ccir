.globl _main
_main:
push %rbp
movq %rsp, %rbp
movq $1, %rax
push %rax
movq $0, %rax
push %rax
movq -8(%rbp), %rax
cmpq $0, %rax
je _label0
movq $1, %rax
movq %rax, -16(%rbp)
_label0:
movq -16(%rbp), %rax
movq %rbp, %rsp
pop %rbp
retq
