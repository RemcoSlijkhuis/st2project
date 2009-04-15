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

	pushl %ebp		#prolog
	movl %esp, %ebp

	movl $0, %eax		#reset aex
	mov PC, %ax		#move the program counter to the lowest 16 bits of eax

	pushl %eax		#push the PC

	movl $0, %eax		#reset eax
	movb IR, %al		#move the IR to the lowest 8 bits of eax

	pushl %eax		#push the IR
	
	pushl $registers	#push the format string
	
	call printf		#print

	movl %ebp, %esp		
	popl %ebp
	ret
