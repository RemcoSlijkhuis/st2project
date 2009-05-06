#############################################
#####CMP: Compare memory and accumulator#####
#############################################
execute_CMP:
	pushl %ebp
	movl %esp, %ebp	

	mov A, %eax
	sub MEM(%ecx), %eax
	cmp %eax, $0
	jl CMP_setcarry
	jmp CMP_clcarry

CMP_setcarry:
	call set_carry_1
	jmp CMP_end

CMP_clcarry:
	call set_carry_0
	jmp CMP_end

CMP_end:
	push %eax
	call check_ZS
	movl %ebp, %esp
	popl %ebp
	ret	

###################################
#####CPX: Compare memory and X#####
###################################
execute_CPX:
	pushl %ebp
	movl %esp, %ebp	

	mov X, %eax
	sub MEM(%ecx), %eax
	cmp %eax, $0
	jl CPX_setcarry
	jmp CPX_clcarry

CPX_setcarry:
	call set_carry_1
	jmp CPX_end

CPX_clcarry:
	call set_carry_0
	jmp CPX_end

CPX_end:
	push %eax
	call check_ZS
	movl %ebp, %esp
	popl %ebp
	ret	

###################################
#####CPY: Compare memory and Y#####
###################################
execute_CPY:
	pushl %ebp
	movl %esp, %ebp	

	mov Y, %eax
	sub MEM(%ecx), %eax
	cmp %eax, $0
	jl CPY_setcarry
	jmp CPY_clcarry

CPY_setcarry:
	call set_carry_1
	jmp CPY_end

CPY_clcarry:
	call set_carry_0
	jmp CPY_end

CPY_end:
	push %eax
	call check_ZS
	movl %ebp, %esp
	popl %ebp
	ret		

#################################
#####CLD: clear decimal flag#####
#################################
execute_CLD:
	pushl %ebp
	movl %esp, %ebp

	mov P, %al		#store processor status in al
	and $0xEF, %al		#clear Decimal flag
	mov %al, P		#store al back into processor status

	movl %ebp, %esp
	popl %ebp
	ret

###############################
#####SED: Set decimal flag#####
###############################
execute_SED:
	pushl %ebp
	movl %esp, %ebp

	mov P, %al		#store processor status in al
	or $0x08, %al		#set Decimal flag
	mov %al, P		#store al back into processor status

	movl %ebp, %esp
	popl %ebp
	ret

#####################################
#####tobcd: Convert a hex to BDC#####
#####################################
tobcd:
	pushl %ebp
	movl %esp, %ebp

	mov 8(%ebp),%eax	##read number
	cmp %al, $0x9		##check wether last number =>a
	jg tobcd_last_adjust	##adjust
	jmp tobcd_first_check	##go check first number

tobcd_last_adjust:
	sub $0x09, %eax		##adjust
	add %eax, $0x10
	cmp %eax, $0x10		##check case was: 0xf*
	jl tobcd_overflow
	jmp tobcd_first_check

tobcd_first_check:
	cmp %eax, $0xa0		##check first number =>a
	jge tobcd_first_adjust	
	jmp tobcd_end

tobcd_first_adjust:
	sub $0x90, %eax		##adjust
	call set_overflow_1	##set overflow flag to 1
	jmp tobcd_end

tobcd_overflow:
	call set_overflow_1	##set overflow flag to 1
	jmp tobcd_end

tobcd_end:
	mov %eax, 8(%ebp)	##adjust number
	movl %ebp, %esp
	popl %ebp
	ret

#######################################
#####frombcd: Convert a BDC to hex#####
#######################################
frombcd:
	pushl %ebp
	movl %esp, %ebp

	mov $0x00, %ebx
	mov 8(%ebp),%eax	##read number
	mov %ah, %bl
	mul %ebx, $10
	add %ebx, %al
	mov %eax, %ebx
	mov %eax, 8(%ebp)	##adjust number

	movl %ebp, %esp
	popl %ebp
	ret