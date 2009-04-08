.global main

main:	movl %esp, %ebp
	jmp start
	movl $0,(%esp)
	call exit
