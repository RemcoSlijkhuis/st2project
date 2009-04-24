	## shift bits one place to the left, LSB is set 0, affects sign and zero flags
	## the bit that is shifted out affects the carry
execute_ASL

	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	movl $0, %eax

	#check if adressing mode is fetch_accumulator
	cmp $0x0000FFFF, %ecx
	ja ASL_acc
	#normal adressing mode
	#get value from memory
	mov MEM(%ecx), %al
	#shift 1 right, save flags for checks and update value
	shl %al, $1
	pushf
	mov %al, MEM(%ecx)	
ASL_checks:
	pushl %eax
	call checkZS
	#retrieve flags and check for carry and set if necessary
	popf	
	jc ASL_setC
	call execute_CLC
	jmp ASL_end
ASL_setC:
	call execute_SEC	
ASL_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
ASL_acc:
	#accumulator adressing mode, value already in cx
	mov %cl %al
	#shift 1 right, save flags for checks and update value
	shl %al, $1
	pushf
	mov %al, A
	jmp ASL_checks
	
	
	##load accumulator with memory, affects negative and zero flags
execute_LDA:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	#get value from memory
	movl $0, %eax
	mov MEM(%ecx), %eax
	#put value in accumulator
	mov %eax, A	
	#check if value is zero or negative and adjust flag accordingly
	pushl %eax
	call check_ZS
		
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
	##load X register with memory, affects negative and zero flags
execute_LDX:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	#get value from memory
	movl $0, %eax
	mov MEM(%ecx), %eax
	#put value in accumulator
	mov %eax, X	
	#check if value is zero or negative and adjust flag accordingly
	pushl %eax
	call check_ZS
		
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
	##load Y register with memory, affects negative and zero flags
execute_LDY:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	#get value from memory
	movl $0, %eax
	mov MEM(%ecx), %eax
	#put value in accumulator
	mov %eax, Y	
	#check if value is zero or negative and adjust flag accordingly
	pushl %eax
	call check_ZS
		
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret

	## shift bits one place to the right, MSB is set 0, affects sign and zero flags
	## the bit that is shifted out affects the carry
execute_LSR

	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	movl $0, %eax

	#check if adressing mode is fetch_accumulator
	cmp $0x0000FFFF, %ecx
	ja LSR_acc
	#normal adressing mode
	#get value from memory
	mov MEM(%ecx), %al
	#shift 1 right, save flags for checks and update value
	shr %al, $1
	pushf
	mov %al, MEM(%ecx)	
LSR_checks:
	pushl %eax
	call checkZS
	#retrieve flags and check for carry and set if necessary
	popf	
	jc LSR_setC
	call execute_CLC
	jmp LSR_end
LSR_setC:
	call execute_SEC	
LSR_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
LSR_acc:
	#accumulator adressing mode, value already in cx
	mov %cl %al
	#shift 1 right, save flags for checks and update value
	shr %al, $1
	pushf
	mov %al, A
	jmp LSR_checks
	
	## shift bits one place to the left, LSB gets carry value, affects sign and zero flags
	## the bit that is shifted out sets new carry	
execute_ROL:

	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	movl $0, %eax

	#check if adressing mode is fetch_accumulator
	cmp $0x0000FFFF, %ecx
	ja ROL_acc
	#normal adressing mode
	#get value from memory
	mov MEM(%ecx), %al
	#shift 1 left, save flags for checks and update value
	rcl %al, $1
	pushf
	mov %al, MEM(%ecx)	
ROL_checks:
	pushl %eax
	call checkZS
	#retrieve flags and check for carry and set if necessary
	popf	
	jc ROL_setC
	call execute_CLC
	jmp ROL_end
ROL_setC:
	call execute_SEC	
ROL_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
ROL_acc:
	#accumulator adressing mode, value already in cx
	mov %cl %al
	#shift 1 left, save flags for checks and update value
	rcl %al, $1
	pushf
	mov %al, A
	jmp ROL_checks
	
	
	## shift bits one place to the right, MSB gets carry value, affects sign and zero flags
	## the bit that is shifted out sets new carry	
execute_ROR:

	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	movl $0, %eax

	#check if adressing mode is fetch_accumulator
	cmp $0x0000FFFF, %ecx
	ja ROR_acc
	#normal adressing mode
	#get value from memory
	mov MEM(%ecx), %al
	#shift 1 right, save flags for checks and update value
	rcr %al, $1
	pushf
	mov %al, MEM(%ecx)	
ROR_checks:
	pushl %eax
	call checkZS
	#retrieve flags and check for carry and set if necessary
	popf	
	jc ROR_setC
	call execute_CLC
	jmp ROR_end
ROR_setC:
	call execute_SEC	
ROR_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
ROR_acc:
	#accumulator adressing mode, value already in cx
	mov %cl %al
	#shift 1 right, save flags for checks and update value
	rcr %al, $1
	pushf
	mov %al, A
	jmp ROR_checks
	

	
	
	

	
	