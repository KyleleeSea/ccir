.globl _main
_main:
movq $100, %rax
neg %rax
push %rax
movq $5, %rax
pop %rcx
movq %rax, %r8
movq %rcx, %rax
cqo
idivq %r8
retq
