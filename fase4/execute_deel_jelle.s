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
	sub %eax, $0x09		##adjust
	add %eax, $0x10
	cmp %eax, $0x10		##check case was: 0xf*
	jl tobcd_overflow
	jmp tobcd_first_check

tobcd_first_check:
	cmp %eax, $0xa0		##check first number =>a
	jge tobcd_first_adjust	
	jmp tobcd_end

tobcd_first_adjust:
	sub %eax, $0x90		##adjust
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