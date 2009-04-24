#Transfer accumulator to X register
execute_TAX:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al	#move accumulator to al register
	mov %al, X	#move al register to X
	push %al
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

#Transfer accumulator to Y register
execute_TAY:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al	#move accumulator to al register
	mov %al, Y	#move al register to Y
	push %al
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret

#exclusive or memory with accumulator
execute_EOR:
	pushl %ebp
	movl %esp, %ebp
	
	mov MEM(%ecx), %bl	#load argument into bl
	xor A, %bl		#xor accumulator and bl
	mov %bl, A		#move bl back to the accumulator
	
	push %bl
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret

#or memory with accumulator
execute_ORA:
	pushl %ebp
	movl %esp, %ebp
	
	mov MEM(%cx), %bl	#load argument into bl
	or A, %bl		#or accumulator and bl
	mov %bl, A		#move bl back to the accumulator
	
	push %bl
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

#push accumulator on stack
execute_PHA:
	pushl %ebp
	movl %esp, %ebp
	
	movw $0x0100,%ax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	mov A, %bl		#store the accumulator in bl
	mov %bl, MEM(%ax)	#store bl in memory on the position indicated by mem pointer ax
	dec %al			#decrease the stack pointer with 1
	mov %al, S		#and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret

#push processor status on stack
execute_PHP:
	pushl %ebp
	movl %esp, %ebp
	
	movw $0x0100,%ax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	mov P, %bl		#store the processor status in bl
	mov %bl, MEM(%ax)	#store bl in memory on the position indicated by mem pointer ax
	dec %al			#decrease the stack pointer with 1
	mov %al, S		#and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull accumulator from stack
execute_PLA:
	pushl %ebp
	movl %esp, %ebp
	
	
	movw $0x0100,%ax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	inc %al			#increase the stack pointer value with 1
	mov %al, S		#and store the stack pointer
	#ax now contains the mem pointer for the location of the new accumulator
	mov MEM(%ax),%bl	#store the value in memory on the position indicated by mem pointer ax in bl
	mov %bl, A		#store bl in the accumulator
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull processor status from stack
execute_PLP:
	pushl %ebp
	movl %esp, %ebp
	
	
	movw $0x0100,%ax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	inc %al			#increase the stack pointer value with 1
	mov %al, S		#and store the stack pointer
	#ax now contains the mem pointer for the location of the new processor status
	mov MEM(%ax),%bl	#store the value in memory on the position indicated by mem pointer ax in bl
	mov %bl, P		#store bl in the processor status
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#store accumulator in memory
execute_STA:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al			#store the accumulator in al
	mov %al, MEM(%cx)		#store al in memory at the given address
	
	movl %ebp, %esp
	popl %ebp
	ret	

#transfer x register into accumulator
execute_TXA:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al	#move the x register to al
	mov %al, A	#move al to the accumulator
	push %al	#push the new value of the accumulator onto the stack
	call check_ZS	#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#store the x register into memory
execute_STX:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al			#store the x register in al
	mov %al, MEM(%cx)		#store al in memory at the given address
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#store the y register into memory
execute_STY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al			#store the y register in al
	mov %al, MEM(%cx)		#store al in memory at the given address
	
	
	movl %ebp, %esp
	popl %ebp
	ret

#transfer stack pointer to x register
execute_TSX:
	pushl %ebp
	movl %esp, %ebp
	
	mov S, %al		#move the stack pointer to al
	mov %al, X		#move al to the x register
	push %al		#push al onto the stack for check_ZS
	call check_ZS		#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#transfer x register to stack pointer
execute_TXS:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al		#move the x register to al
	mov %al, S		#move al to the stack pointer
	push %al		#push al onto the stack for check_ZS
	call check_ZS		#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#transfer y register to accumulator
execute_TYA:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al		#move the y register to al
	mov %al, A		#move al to the accumulator
	push %al		#push al onto the stack for check_ZS
	call check_ZS		#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#subtract memory from accumulator with borrow
execute_SBC:
	pushl %ebp
	movl %esp, %ebp
	
	mov P, %al		#move the processor status to al
	and $0x1, %al		#make al 0x1 if carry, 0x0 if no carry
	cmp $0, %al		#if no carry
	jmp SBC_nocarry
	stc			#set carry
	jmp SBC_end
	SBC_nocarry:	#if no carry,
	clc		#clear carry
	SBC_end:
	
	mov MEM(%cx), %bl	#load argument into bl
	mov A, %dl		#load accumulator into dl
	sbb %dl, %bl		#subtract dl with bl
	
	jc SBC_setcarry		#set 6502 if x86 carry is set
	#clear carry
	mov P, %al		#load processor status into al
	and $0xFE, %al		#clear carry flag
	mov %al, P		#store processor status
	jmp SBC_setcarryend
	SBC_setcarry:
	#set carry
	mov P, %al		#load processor status into al
	or $0x01, %al		#set carry flag
	mov %al, P		#store processor status
	SBC_setcarryend:
	
	movl %ebp, %esp
	popl %ebp
	ret	