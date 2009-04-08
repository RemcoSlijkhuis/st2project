##################################################################
#######################Group 2 Project ST 2#######################
##################################################################

.text
registers: .asciz "A:  %d\nX:  %d\nY:  %d\nPC: %d\nS:  %d\nIR: %d\nP:  %d\n"

.global showi

##################################################
######### showi: Print out the registers #########
##################################################
showi:	pushl %ebp
	movl %esp, %ebp

	pushl P,IR,S,PC,Y,X,A
	pushl registers
	call printf

	movl %ebp, %esp
	popl %ebp
	ret
