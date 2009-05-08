	##checks if carry flag is set, if not branch is taken, else execution resumes normally.
execute_BCC:
	pushl %ebp
	movl %esp, %ebp
	
	#check if carry is set
	and $0x01, P
	cmp $0x0, P
	#jump to end if carry is set
	jne BCC_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BCC_end:
	movl %ebp, %esp
	popl %ebp
	ret

	##checks if carry flag is set, if so branch is taken, else execution resumes normally.
execute_BCS:
	pushl %ebp
	movl %esp, %ebp
	
	#check if carry is set
	and $0x01, P
	cmp $0x0, P
	#jump to end if carry is not set
	je BCS_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BCS_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if zero flag is set, if so branch is taken, else execution resumes normally.
execute_BEQ:
	pushl %ebp
	movl %esp, %ebp
	
	#check if zero flag is set
	and $0x02, P
	cmp $0x0, P
	#jump to end if zero flag is not set
	je BEQ_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BEQ_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if memory adress and accumulator are different for each bit by performing an AND operation, without changing them
	##the negative flag is set equal to bit 7 of the memory value and the overflow value is set equals to bit 6 of the memory value
	## the zero flag is set according to the result of the AND operation (1 if each bit is not equal)
execute_BIT:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	#retrieve memory value and accumulator value and perform AND operation
	movl $0, %eax
	movl $0, %ebx
	mov A, %al
	mov MEM(ecx), %bl
	AND %bl, %al
	#let check_ZS decide if zero
	pushl %eax
	call check_ZS
	popl %eax
	#retrieve bit 7 of memory value and set negative flag accordingly
	AND $0x80, %bl
	cmp $0, %bl
	jne BIT_negative
	#value is negative, set flag
	AND $0x7F, P
	j Bit_OVtest
BIT_negative:
	#value is positive, clear flag
	OR $0x80, P
BIT_OVtest:
	#retrieve bit 6 of memory value and set overflow flag accordingly
	AND $0x40, %bl
	cmp $0, %bl
	je BIT_OV
	#bit is 0, clear overflow flag 
	call set_overflow_0
	j BIT_end
BIT_OV:
	#bit is 1, set overflow flag
	call set_overflow_1
BIT_end:
	#no return or changed values
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp	
	ret
	
	##checks if negative flag is set, if so branch is taken, else execution resumes normally.
execute_BMI:
	pushl %ebp
	movl %esp, %ebp
	
	#check if negative flag is set
	and $0x80, P
	cmp $0x0, P
	#jump to end if negative flag is not set
	je BMI_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BMI_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if zero flag is set, if not branch is taken, else execution resumes normally.
execute_BNE:	
	pushl %ebp
	movl %esp, %ebp
	
	#check if zero flag is set
	and $0x02, P
	cmp $0x0, P
	#jump to end if zero is set
	jne BNE_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BNE_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if negative flag is set, if not branch is taken, else execution resumes normally.
execute_BPL:
	pushl %ebp
	movl %esp, %ebp
	
	#check if negative flag is set
	and $0x80, P
	cmp $0x0, P
	#jump to end if negative flag is set
	jne BPL_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BPL_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	#runs interrupt subroutine, after pushing current SP+2 on stack, low byte first and pushing the status register and 0x10.
execute_BRK:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	movl $0, %eax
	movl $0, %ebx
	#load current stack pointer
	mov S, %bl
	#push PC+2 on stack, low byte first
	mov PC, %ax
	add $2, %ax
	mov %al, MEM(%bl)
	dec %bl
	mov %ah, MEM(%bl)
	dec %bl
	#push processor status + 00010000B on stack
	mov P, MEM(%bl)
	dec %bl
	mov $0x10 MEM(%bl)
	dec %bl
	#update S
	mov %bl, S
	#load adress off interrupt handler in PC
	movl $0, %eax
	movl $0, %ebx
	#retrive low byte off handler
	movl $0xFFFE, %ax
	mov MEM(%ax), %bl
	#retrieve high byte off handler
	movl $0xFFFF, %ax
	mov MEM(%ax), %bh
	mov %bx, PC
	#adjust PC, because fetch ends with incrementing PC.
	decl PC
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
##checks if overflow flag is set, if not branch is taken, else execution resumes normally.
execute_BVC:
	pushl %ebp
	movl %esp, %ebp
	
	#check if overflow flag is set
	and $0x40, P
	cmp $0x0, P
	#jump to end if overflow flag is set
	jne BVC_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BVC_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
##checks if overflow flag is set, if so branch is taken, else execution resumes normally.
execute_BVS:
	pushl %ebp
	movl %esp, %ebp
	
	#check if overflow flag is set
	and $0x40, P
	cmp $0x0, P
	#jump to end if overflow flag is not set
	je BVS_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BVS_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
##Sets the interrupt disable bit to 0
execute_CLI:
	pushl %ebp
	movl %esp, %ebp
	
	#set bit 2 to zero
	and $0xfb,P
	
	movl %ebp, %esp
	popl %ebp
	ret
	
##jumps by setting the program counter to a new value
execute_JMP:

	pushl %ebp
	movl %esp, %ebp
	#load new program counter
	movl %cx, PC
	
	movl %ebp, %esp
	popl %ebp
	ret
	
##jumps by setting the program counter ta a new value, stores second byte after jsr instruction on stack
execute_JSR
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	#push PC +  on (6502)stack (PC already points at second byte, due to fetch_abs subroutine
	movl $0, %eax
	movl $0, %ebx
	movl PC, %eax 
	#load stack pointer
	mov S, %bl
	mov %al, MEM(%bl)
	dec %bl
	mov %ah, MEM(%bl)
	#update stack pointer
	mov %bl, S
	
	#load new program counter
	movl %cx, PC
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
##return program control after (any)interrupt handling
##Processor status is restored from stack and PC is restored from stack
execute_RTI:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	#pull processor status from stack
	call execute_PLP
	#pull program counter from stack
	movl $0x04, %eax
	movl $0, %ebx
	mov S, %al
	#get low byte
	mov MEM(%eax), %bl
	incl %eax
	#get high byte
	mov MEM(%eax), %bh
	incl %eax
	#load PC and restore S
	mov %bx, PC
	mov %al, S
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
##return from subroutine, restores PC from stack
exectue_RTS:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	#pull program counter from stack
	movl $0x04, %eax
	movl $0, %ebx
	mov S, %al
	#get low byte
	mov MEM(%eax), %bl
	incl %eax
	#get high byte
	mov MEM(%eax), %bh
	incl %eax
	#load PC and restore S
	mov %bx, PC
	mov %al, S
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
##sets interrupt disable flag
execute_SEI:
	pushl %ebp
	movl %esp, %ebp
	
	#set bit 2 to zero
	or $0x04,P
	
	movl %ebp, %esp
	popl %ebp
	ret	
	
	
	
	
	

	
	
	
	 
	
	
	
	


	