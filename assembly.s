.globl _main
_main:
movq $3, %rax
push %rax
movq $4, %rax
pop %rcx
subq %rax, %rcx
movq %rcx, %rax
ret
