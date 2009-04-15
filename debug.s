##################################################################
#######################Group 2 Project ST 2#######################
##################################################################

.text
registers: .asciz "IR:  %d\nP:  %d\n"

.global showi

##################################################
######### showi: Print out the registers #########
##################################################
showi:	pushl %ebp
	movl %esp, %ebp

	movl $0, %eax
	movl P, %ax

	pushl %eax

	movl $0, %eax
	movl IR, %al

	pushl %eax
	
	pushl registers
	call printf

	movl %ebp, %esp
	popl %ebp
	ret
