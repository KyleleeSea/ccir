.globl _main
_main:
push %rbp
movq %rsp, %rbp
movq $0, %rax
push %rax
movq $0, %rax
cmpq $0, %rax
je _label0
movq $0, %rax
cmpq $0, %rax
je _label2
movq $3, %rax
movq %rax, -8(%rbp)
jmp _label3
_label2:
movq $4, %rax
movq %rax, -8(%rbp)
_label3:
jmp _label1
_label0:
movq $1, %rax
movq %rax, -8(%rbp)
_label1:
movq -8(%rbp), %rax
movq %rbp, %rsp
pop %rbp
retq
addq $8, %rsp
