##################################################################
#######################Groep 2 Project ST 2#######################
##################################################################

.text
Registers: .asciz "A:  %d\nX:  %d\nY:  %d\nPC: %d\nS:  %d\nIR: %d\nP:  %d\n"

.global showi

#################################################
######### showi: Print de registers uit #########
#################################################
showi:	pushl %ebp
	movl %esp, %ebp

	pushl P,IR,S,PC,Y,X,A
	pushl Registers
	call printf

	movl %ebp, %esp
	popl %ebp
	ret
