##################################################################
#######################Group 2 Project ST 2#######################
##################################################################

.text
 registers: .asciz "IR:  0x%X\tPC:  0x%X\n"
 showrregisters: .asciz "A: 0x%X\t X: 0x%X\t Y: 0x%X\t S: 0x%X\tP: 0x%X\n"
 operand: .asciz "Operand Address: 0x%X\n"

.global showi
.global showr
.global showo

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

	movl %ebp, %esp		#restore stack pointer
	popl %ebp
	ret



showr:
	
	pushl %ebp		#prolog
	movl %esp, %ebp

	movl $0, %eax		#Clear eax
	movb P, %al		#move P to eax

	pushl %eax		#push P

	movb S, %al		#move S to eax
	pushl %eax		#push S

	movb Y, %al		#move Y to eax
	pushl %eax		#push Y

	movb X, %al		#move X to eax
	pushl %eax		#push X

	movb A, %al		#move A to eax
	pushl %eax		#push A
		
	pushl $showrregisters	#push showr string
	
	call printf		#call printf routine

	movl %ebp, %esp		#restore stack pointer
	popl %ebp

	ret

showo:
	pushl %ebp		#prolog
	movl %esp, %ebp

	
	pushl %ecx		#push ecx
	pushl $operand		#push Operand-string

	call printf		#Print Operand value


	movl %ebp, %esp		#restore stack pointer
	popl %ebp

	ret






	