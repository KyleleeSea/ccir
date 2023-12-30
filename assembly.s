.globl _main
_main:
movl $4, %eax
push %rax
movl $2, %eax
pop %rcx
movl %rax, %rbx
movl %rcx, %rax
cdq
idivl %rbx
ret
