##################################################################
#######################Group 2 Project ST 2#######################
##################################################################

.text
 registers: .asciz "IR:  %x\tPC:  %x\n"

.global showi

##################################################
######### showi: Print out the registers #########
##################################################
showi:	

	pushl %ebp
	movl %esp, %ebp

	movl $0, %eax
	mov PC, %ax

	pushl %eax

	movl $0, %eax
	movb IR, %al

	pushl %eax
	
	pushl $registers
	
	call printf

	movl %ebp, %esp
	popl %ebp
	ret
