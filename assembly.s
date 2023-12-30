.globl _main
_main:
movl $1, %eax
push %rax
movl $2, %eax
pop %rcx
subl %rax, %rcx
push %rax
movl $3, %eax
pop %rcx
subl %rax, %rcx
ret
