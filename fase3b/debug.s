##################################################################
#######################Group 2 Project ST 2#######################
##################################################################

.text
 registers: .asciz "IR:  0x%X\tPC:  0x%X\t->\t"
 showrregisters: .asciz "A: 0x%X\t X: 0x%X\t Y: 0x%X\t S: 0x%X\tP: 0x%X\n"

.global showi
.global showr

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



showr:
	
	pushl %ebp		#prolog
	movl %esp, %ebp

	movl $0, %eax
	movb P, %al

	pushl %eax

	movb S, %al
	pushl %eax

	movb Y, %al
	pushl %eax

	movb X, %al
	pushl %eax

	movb A, %al
	pushl %eax
		
	pushl $showrregisters
	
	call printf

	movl %ebp, %esp
	popl %ebp

	ret


	