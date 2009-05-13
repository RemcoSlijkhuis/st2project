
.data


.global execute_ADC
.global execute_AND
.global execute_ASL
.global execute_BCC
.global execute_BCS
.global execute_BEQ
.global execute_BIT
.global execute_BMI
.global execute_BNE
.global execute_BPL
.global execute_BRK
.global execute_BVC
.global execute_BVS
.global execute_CLC
.global execute_CLD
.global execute_CLI
.global execute_CLV
.global execute_CMP
.global execute_CPX
.global execute_CPY
.global execute_DEC
.global execute_DEX
.global execute_DEY
.global execute_EOR
.global execute_INC
.global execute_INX
.global execute_INY
.global execute_JMP
.global execute_JSR
.global execute_LDA
.global execute_LDX
.global execute_LDY
.global execute_LSR
.global execute_NOP
.global execute_ORA
.global execute_PHA
.global execute_PHP
.global execute_PLA
.global execute_PLP
.global execute_ROL
.global execute_ROR
.global execute_RTI
.global execute_RTS
.global execute_SBC
.global execute_SEC
.global execute_SED
.global execute_SEI
.global execute_STA
.global execute_STP
.global execute_STX
.global execute_STY
.global execute_TAX
.global execute_TAY
.global execute_TSX
.global execute_TXA
.global execute_TXS
.global execute_TYA

		## execute subroutines

##################################################
#################ADC: A+MEM+carry#################
##################################################
execute_ADC:
	pushl %ebp
	movl %esp, %ebp
	

	movl $0, %ebx
	mov MEM(%ecx), %bl	#load argument into bl
	
	movl $0, %eax
	mov A, %al		#load accumulator into dl
	mov A, %ah
	
	movl $0, %edx
	mov P, %dl
	
	andb $0x08, %dl
	cmp $0, %dl
	je ADC_continue

	pushl %eax
	call frombcd
	popl %eax
	movb %al, %ah

	pushl %ebx
	call frombcd
	popl %ebx

ADC_continue:
	call load_C		#set the x86 carry flag	
	

	adc %bl, %al		#add bl to al
	pushf
	mov %al, A	#store dl back in the accumulator
	
	#set the carry flag to either 1 or 0
	mov $0xFF, %cx			#maximum value for non-BCD mode
	mov P, %dl
	andb $0x08, %dl
	cmp $0, %dl
	je ADC_end2
	mov $99, %cx			#maximum value for BCD mode
ADC_end2:
	mov %ah, %al
	mov $0, %ah
	adc %bx, %ax
	cmp %cx, %ax
	ja ADC_carry
	call set_carry_0
	jmp ADC_carry_end
	ADC_carry:
	call set_carry_1
	movw $0, %ax
	mov A, %al
	movb $100, %bl
	divb %bl
	movb %ah, A
	ADC_carry_end:
	
	
	#store the x86 overflow into the 6052 flags
	call check_O		
	
	#properly set the zero and negative flags in the 6052 processor status
	push A
	call check_ZS
	
	
	
	movl $0, %edx
	mov P, %dl
	andb $0x08, %dl
	cmp $0, %dl
	je ADC_end


	movl $0, %eax
	movb A, %al
	pushl %eax
	call tobcd
	popl %eax
	movb %al, A

	
	

ADC_end:
	
	movl %ebp, %esp
	popl %ebp
	ret

######################################
#####AND: AND mem and Accumulator#####
###################################### 
execute_AND:
	pushl %ebp
	movl %esp, %ebp

	mov MEM(%ecx), %al
	and %al, A
	push A
	call check_ZS	##adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret
	


	## shift bits one place to the left, LSB is set 0, affects sign and zero flags
	## the bit that is shifted out affects the carry
execute_ASL:

	pushl %ebp
	movl %esp, %ebp
	#movl $0xFFFF, %eax
	#mov %al, X

	#check if adressing mode is fetch_accumulator
	cmpl $0x0000FFFF, %ecx
	ja ASL_acc
	#normal adressing mode
	#get value from memory
	mov MEM(%ecx), %al
	#shift 1 right, save flags for checks and update value
	shl $1,%al
	pushf
	mov %al, MEM(%ecx)
ASL_checks:
	pushl %eax
	call check_ZS
	popl %eax
	#retrieve flags and check for carry and set if necessary
	popf	
	jc ASL_setC
	call set_carry_0
	jmp ASL_end
ASL_setC:
	call set_carry_1	
ASL_end:
	#restore values and return
	movl %ebp, %esp
	popl %ebp
	ret
ASL_acc:
	#accumulator adressing mode, value already in cx
	mov %cl, %al
	#shift 1 right, save flags for checks and update value
	shl $1,%al
	pushf
	mov %al, A
	jmp ASL_checks



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
	decb PC 		#decrease program counter with 1

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
	decb PC 		#decrease program counter with 1

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

	decb PC

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
	mov MEM(%ecx), %bl
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
	jmp BIT_OVtest
BIT_negative:
	#value is positive, clear flag
	orb $0x80, P
BIT_OVtest:
	#retrieve bit 6 of memory value and set overflow flag accordingly
	AND $0x40, %bl
	cmp $0, %bl
	je BIT_OV
	#bit is 0, clear overflow flag 
	call set_overflow_0
	jmp BIT_end
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
	movl $0, %ebx
	mov P, %bl
	andb $0x80, %bl
	cmp $0x0, %bl
	#jump to end if negative flag is not set
	je BMI_end
	#change PC to simulate jump
	mov %cx, PC
	decb PC
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
	movl $0, %ebx
	mov P, %bl
	and $0x02, %bl
	cmp $0x0, %bl
	#jump to end if zero is set
	jne BNE_end
	#change PC to simulate jump
	mov %cx, PC
	decb PC

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
	movl $0, %ebx
	mov P, %bl
	andb $0x80, %bl
	cmp $0x0, %bl
	#jump to end if negative flag is set
	jne BPL_end
	#change PC to simulate jump
	mov %cx, PC
	
	decb PC

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
	movl $0x0100, %ebx
	#load current stack pointer
	mov S, %bl
	#push PC+2 on stack, low byte first
	mov PC, %ax
	add $2, %ax
	mov %al, MEM(%ebx)
	dec %bl
	mov %ah, MEM(%ebx)
	dec %bl


	#push processor status + 00010000B on stack
	mov P, %dl
	mov %dl, MEM(%ebx)
	dec %bl
	movb $0x10, MEM(%ebx)
	dec %bl
	#update S
	mov %bl, S
	#load adress off interrupt handler in PC
	movl $0, %eax
	movl $0, %ebx
	#retrive low byte off handler
	movw $0xFFFE, %ax
	mov MEM(%eax), %bl
	#retrieve high byte off handler
	movw $0xFFFF, %ax
	mov MEM(%eax), %bh
	mov %bx, PC
	#adjust PC, because fetch ends with incrementing PC.
	decb PC
	
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
	movl $0, %ebx
	mov P, %bl
	and $0x40, %bl
	cmp $0x0, %bl
	#jump to end if overflow flag is set
	jne BVC_end
	#change PC to simulate jump
	mov %cx, PC

	decb PC
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
	movl $0, %ebx
	mov P, %bl
	and $0x40, %bl
	cmp $0x0, %bl
	#jump to end if overflow flag is not set
	je BVS_end
	#change PC to simulate jump
	mov %cx, PC
	decb PC

	#restore stack pointer and return
BVS_end:
	movl %ebp, %esp
	popl %ebp
	ret


#################################################################
######################CLC: Clear carry flag######################
#################################################################
execute_CLC:
	pushl %ebp
	movl %esp, %ebp
	
	mov P, %al
	and $0xFE, %al	##change the first bit to 0
	mov %al, P

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
	and $0xF7, %al		#clear Decimal flag
	mov %al, P		#store al back into processor status

	movl %ebp, %esp
	popl %ebp
	ret
	

##Sets the interrupt disable bit to 0
execute_CLI:
	pushl %ebp
	movl %esp, %ebp
	
	#set bit 2 to zero
	andb $0xfb,P
	
	movl %ebp, %esp
	popl %ebp
	ret

	

#################################################################
####################CLV: Clear overflow flag#####################
#################################################################
execute_CLV:
	pushl %ebp
	movl %esp, %ebp

	mov P, %al
	and $0xBF, %al	##change the carry flag
	mov %al, P
	
	movl %ebp, %esp
	popl %ebp
	ret
		

#############################################
#####CMP: Compare memory and accumulator#####
#############################################
execute_CMP:
	pushl %ebp
	movl %esp, %ebp	

	movl $0, %ebx
	mov A, %bl
	sub MEM(%ecx), %ebx
	cmp $0, %ebx
	jl CMP_setcarry
	jmp CMP_clcarry

CMP_setcarry:
	call set_carry_1
	jmp CMP_end

CMP_clcarry:
	call set_carry_0
	jmp CMP_end

CMP_end:
	push %ebx
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
	
	movl $0, %ebx
	mov X, %bl
	sub MEM(%ecx), %ebx
	cmp $0, %ebx
	jl CPX_setcarry
	jmp CPX_clcarry

CPX_setcarry:
	call set_carry_1
	jmp CPX_end

CPX_clcarry:
	call set_carry_0
	jmp CPX_end

CPX_end:
	push %ebx
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

	movl $0, %ebx
	mov Y, %bl
	sub MEM(%ecx), %ebx
	cmp $0, %ebx
	jl CPY_setcarry
	jmp CPY_clcarry

CPY_setcarry:
	call set_carry_1
	jmp CPY_end

CPY_clcarry:
	call set_carry_0
	jmp CPY_end

CPY_end:
	push %ebx
	call check_ZS
	movl %ebp, %esp
	popl %ebp
	ret		


#################################################################
#######################DEC: Lower mem with 1#######################
#################################################################
execute_DEC:
	pushl %ebp
	movl %esp, %ebp

	mov MEM(%ecx), %al
	dec %al
	mov %al, MEM(%ecx)
	
	push MEM(%ecx)
	call check_ZS	##adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
#######################DEX: Lower X with 1#######################
#################################################################
execute_DEX:
	pushl %ebp
	movl %esp, %ebp

	mov X, %al
	dec %al
	mov %al, X
	push X
	call check_ZS	##adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
#######################DEY: Lower Y with 1#######################
#################################################################
execute_DEY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al
	dec %al
	mov %al, Y
	push Y
	call check_ZS	##adjust zero en neg. flags

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
	
	push A
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	


#increments memory by one
execute_INC:
	pushl %ebp
	movl %esp, %ebp
	
	movl $0, %ebx
	
	mov MEM(%ecx), %bl	#load old value from memory
	inc %bl			#increment with one
	mov %bl, MEM(%ecx)	#store back in memory
	
	push %ebx
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	


#increment x register by one
execute_INX:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al
	inc %al
	mov %al, X
	
	push X
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	
#increment y register by one
execute_INY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al
	inc %al
	mov %al, Y
	
	push Y
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

##jumps by setting the program counter to a new value
execute_JMP:

	pushl %ebp
	movl %esp, %ebp
	#load new program counter
	movw %cx, PC

	decb PC
	
	movl %ebp, %esp
	popl %ebp
	ret


##jumps by setting the program counter ta a new value, stores second byte after jsr instruction on stack
execute_JSR:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	#push PC +  on (6502)stack (PC already points at second byte, due to fetch_abs subroutine
	movl $0, %edx
	movl $0x0100, %ebx
	movw PC, %dx 
	#load stack pointer
	mov S, %bl
	mov %dl, MEM(%ebx)
	dec %bl	
	mov %dh, MEM(%ebx)
	#update stack pointer
	dec %bl
	mov %bl, S
	
	#load new program counter
	movw %cx, PC

	decb PC
	

	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret	





	##load accumulator with memory, affects negative and zero flags
execute_LDA:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	#get value from memory
	movl $0, %eax
	mov MEM(%ecx), %al
	#put value in accumulator
	mov %al, A	
	#check if value is zero or negative and adjust flag accordingly
	push A
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
	mov MEM(%ecx), %al
	#put value in accumulator
	mov %al, X	
	#check if value is zero or negative and adjust flag accordingly
	push X
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
	mov MEM(%ecx), %al
	#put value in accumulator
	mov %al, Y	
	#check if value is zero or negative and adjust flag accordingly
	push Y
	call check_ZS
		
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret




	## shift bits one place to the right, MSB is set 0, affects sign and zero flags
	## the bit that is shifted out affects the carry
execute_LSR:

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
	shr $1, %al
	pushf
	mov %al, MEM(%ecx)	
LSR_checks:
	pushl %eax
	call check_ZS
	#retrieve flags and check for carry and set if necessary
	popl %eax
	popf	
	jc LSR_setC
	call set_carry_0
	jmp LSR_end
LSR_setC:
	call set_carry_1	
LSR_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
LSR_acc:
	#accumulator adressing mode, value already in cx
	mov %cl, %al
	#shift 1 right, save flags for checks and update value
	shr $1, %al
	pushf
	mov %al, A
	jmp LSR_checks





#no operation
execute_NOP:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

#or memory with accumulator
execute_ORA:
	pushl %ebp
	movl %esp, %ebp
	
	mov MEM(%ecx), %bl	#load argument into bl
	or A, %bl		#or accumulator and bl
	mov %bl, A		#move bl back to the accumulator
	
	push A
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

#push accumulator on stack
execute_PHA:
	pushl %ebp
	movl %esp, %ebp
	
	mov $0x0100,%eax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	mov A, %bl		#store the accumulator in bl
	mov %bl, MEM(%eax)	#store bl in memory on the position indicated by mem pointer ax
	dec %al			#decrease the stack pointer with 1
	mov %al, S		#and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret

#push processor status on stack
execute_PHP:
	pushl %ebp
	movl %esp, %ebp
	
	mov $0x0100,%eax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	mov P, %bl		#store the processor status in bl
	mov %bl, MEM(%eax)	#store bl in memory on the position indicated by mem pointer ax
	dec %al			#decrease the stack pointer with 1
	mov %al, S		#and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull accumulator from stack
execute_PLA:
	pushl %ebp
	movl %esp, %ebp
	
	
	mov $0x0100,%eax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	inc %al			#increase the stack pointer value with 1
	mov %al, S		#and store the stack pointer
	#ax now contains the mem pointer for the location of the new accumulator
	mov MEM(%eax),%bl	#store the value in memory on the position indicated by mem pointer ax in bl
	mov %bl, A		#store bl in the accumulator
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull processor status from stack
execute_PLP:
	pushl %ebp
	movl %esp, %ebp
	
	
	mov $0x0100,%eax	#set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al		#set the last byte of the mem pointer ax to the stack pointer value
	inc %al			#increase the stack pointer value with 1
	mov %al, S		#and store the stack pointer
	#ax now contains the mem pointer for the location of the new processor status
	mov MEM(%eax),%bl	#store the value in memory on the position indicated by mem pointer ax in bl
	mov %bl, P		#store bl in the processor status
	
	
	movl %ebp, %esp
	popl %ebp
	ret	



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
	call load_C
	mov $0, %eax
	mov MEM(%ecx), %al
	
	#shift 1 left, save flags for checks and update value
	rcl $1, %ax
	mov %al, MEM(%ecx)
	mov %al, %dl		
	
	cmp $0x1, %ah	#if carry has to be set
	jge ROL_setC	#then jump to ROL_setC
	call set_carry_0	#otherwise, set carry to 0
ROL_checks:
	movl $0, %eax
	mov %dl, %al
	pushl %eax
	call check_ZS
	jmp ROL_end
ROL_setC:
	call set_carry_1
	jmp ROL_checks
ROL_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
ROL_acc:
	call load_C
	#accumulator adressing mode, value already in cx
	movl $0, %eax
	mov %cl, %al
	#shift 1 left, save flags for checks and update value
	rcl $1,%ax
	
	mov %al, A
	mov %al, %dl
	
	cmp $0x1, %ah	#if carry has to be set
	jge ROL_setC	#then jump to ROL_setC
	call set_carry_0	#otherwise, set carry to 0
	
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
	call load_C
	mov MEM(%ecx), %al
	#shift 1 right, save flags for checks and update value
	rcr $1,%al
	pushf
	mov %al, MEM(%ecx)	
ROR_checks:
	pushl %eax
	call check_ZS
	#retrieve flags and check for carry and set if necessary
	popl %eax
	popf	
	jc ROR_setC
	call set_carry_0
	jmp ROR_end
ROR_setC:
	call set_carry_1
ROR_end:
	#restore values and return
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
ROR_acc:
	#accumulator adressing mode, value already in cx
	call load_C	
	mov %cl, %al
	#shift 1 right, save flags for checks and update value
	rcr $1,%al
	pushf
	mov %al, A
	jmp ROR_checks
	

	
	
		

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
	movl $0x0100, %eax
	movl $0, %ebx
	mov S, %al
	#get low byte
	incl %eax
	mov MEM(%eax), %bh
	
	#get high byte
	incl %eax
	mov MEM(%eax), %bl
	
	#load PC and restore S
	mov %bx, PC
	mov %al, S
	decb PC	

	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret



##return from subroutine, restores PC from stack
execute_RTS:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	#pull program counter from stack
	movl $0x0100, %eax
	movl $0, %ebx
	mov S, %al
	#get low byte
	incl %eax
	mov MEM(%eax), %bh
	
	#get high byte
	incl %eax	
	mov MEM(%eax), %bl
	
	#load PC and restore S
	mov %bx, PC
	

	mov %al, S
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	

#subtract memory from accumulator with borrow
execute_SBC:
	pushl %ebp
	movl %esp, %ebp
	
	movl $0, %ebx
	movl $0, %edx

	
	mov MEM(%ecx), %bl	#load argument into bl
	mov A, %dl		#load accumulator into dl

	movl $0, %eax
	mov P, %al
	andb $0x08, %al
	cmp $0, %al
	je SBC_continue

	pushl %edx
	call frombcd
	popl %edx

	pushl %ebx
	call frombcd
	popl %ebx


SBC_continue:
	call load_C		#set the x86 carry flag
	call swap_carry	#account for x86 carry/borrow

	sbb %bl, %dl		#subtract dl with bl
	pushf
	mov %dl, A	#store dl back in the accumulator
	
	
	#store the x86 overflow and carry flags into the 6052 flags
	popf
	call swap_carry	#account for x86 carry/borrow
	pushf
	call check_CO		
	
	#properly set the zero and negative flags in the 6052 processor status
	push A
	call check_ZS
	
	movl $0, %eax
	mov P, %al
	andb $0x08, %al
	cmp $0, %al
	je SBC_end

	pushl %edx
	call tobcd
	popl %edx
	mov %dl, A



SBC_end:
	movl %ebp, %esp
	popl %ebp
	ret		

#set carry flag
execute_SEC:
	pushl %ebp
	movl %esp, %ebp
	
	mov P, %al
	or $0x01, %al	##change the first bit to 1
	mov %al, P
	
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



	
##sets interrupt disable flag
execute_SEI:
	pushl %ebp
	movl %esp, %ebp
	
	#set bit 2 to zero
	or $0x04,P
	
	movl %ebp, %esp
	popl %ebp
	ret


#store accumulator in memory
execute_STA:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al			#store the accumulator in al
	mov %al, MEM(%ecx)		#store al in memory at the given address
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_STP:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

#store the x register into memory
execute_STX:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al			#store the x register in al
	mov %al, MEM(%ecx)		#store al in memory at the given address
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#store the y register into memory
execute_STY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al			#store the y register in al
	mov %al, MEM(%ecx)		#store al in memory at the given address
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#Transfer accumulator to X register
execute_TAX:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al	#move accumulator to al register
	mov %al, X	#move al register to X
	push X
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
	push Y
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret

#transfer stack pointer to x register
execute_TSX:
	pushl %ebp
	movl %esp, %ebp
	
	mov S, %al		#move the stack pointer to al
	mov %al, X		#move al to the x register
	push X			#push al onto the stack for check_ZS
	call check_ZS		#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret		

#transfer x register into accumulator
execute_TXA:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al	#move the x register to al
	mov %al, A	#move al to the accumulator
	push A		#push the new value of the accumulator onto the stack
	call check_ZS	#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret

	
#transfer x register to stack pointer
execute_TXS:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al		#move the x register to al
	mov %al, S		#move al to the stack pointer
	push S			#push al onto the stack for check_ZS
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
	push A			#push al onto the stack for check_ZS
	call check_ZS		#set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	
	
	
	






##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##
##!!!!!!!!!Extra functions!!!!!!!!!##
##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!##

################################################################################################
##########check_ZS: Checks and changes the negative and zero flag for the pushed value##########
################################################################################################
check_ZS:
	pushl %ebp
	movl %esp, %ebp

	mov P, %bl
	
	mov 8(%ebp),%al
	cmp $0, %al
	jz set_zero		##Declare the zero flag true, the negative flag unknown
	js set_neg		##Declare the zero flag false, the negative flag true 
	jmp set_none		##Declare the zero flag false and the negative flag false 

set_zero:
	js set_both		##set both zero flag and negative flag to true
	or $0x02, %bl		##zero flag true
	and $0x7F, %bl 		##negative flag false
	jmp set_end

set_neg:
	and $0xFD, %bl		##zero flag false
	or $0x80, %bl		##negative flag true
	jmp set_end

set_none:
	and $0xFD, %bl		##zero flag false
	and $0x7F, %bl 		##zero flag false
	jmp set_end
set_both:
	or $0x02, %bl		##zero flag true
	or $0x80, %bl		##negative flag true
	jmp set_end		
set_end:			##close the function
	mov %bl, P
	movl %ebp, %esp
	popl %ebp
	ret

swap_carry:
	jc SC_carry	#if carry is set, jump to SC_carry
	stc		#carry is not set, so set it
	jmp SC_end
	SC_carry:	#carry is set, so:
	clc		#clear carry
	SC_end:
	ret
	
	
#check for carry and overflow flags, requires processor status pushed on the stack
check_CO:
	pushl %ebp
	movl %esp, %ebp

	mov 8(%ebp),%eax
	push %eax
	popf
	
	jo CO_overflow		#if there's overflow, set overflow flag to 1
	
	#overflow flag set to 0:
	jc CO_noverflow_carry	#if x86 carry = 0, set carry to 0
	call set_overflow_0
	call set_carry_0
	jmp CO_overflow_end
	
	CO_noverflow_carry:
	call set_overflow_0	
	call set_carry_1
	jmp CO_overflow_end	
	
	#overflow flag set to 1:
	CO_overflow:
	jc CO_overflow_carry
	call set_overflow_1	#set overflow flag to 1
	call set_carry_0
	jmp CO_overflow_end
	
	CO_overflow_carry:
	call set_overflow_1	#set overflow flag to 1
	call set_carry_1	
	CO_overflow_end:
	
	movl %ebp, %esp
	popl %ebp
	ret

#check for overflow flags, requires processor status pushed on the stack
check_O:
	pushl %ebp
	movl %esp, %ebp

	mov 8(%ebp),%eax
	push %eax
	popf
	
	jo O_overflow		#if there's overflow, set overflow flag to 1
	
	#overflow flag set to 0:
	call set_overflow_0
	jmp O_overflow_end
	
	#overflow flag set to 1:
	O_overflow:
	call set_overflow_1	#set overflow flag to 1
	
	O_overflow_end:
	
	movl %ebp, %esp
	popl %ebp
	ret

#loads the carry flag from the 6052 processor status into the x86 flag
load_C:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	mov P, %al		#move the processor status to al
	and $0x1, %al		#make al 0x1 if carry, 0x0 if no carry
	cmp $0x0, %al		
	je LoadC_nocarry	#if carry flag is 0, set no carry
	stc			#set x86 carry
	jmp LoadC_end
	LoadC_nocarry:		#if no carry,
	clc			#set x86 carry to 0
	LoadC_end:
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret

set_overflow_0:
	mov P, %al		#store processor status in al
	and $0xBF, %al		#set overflow flag
	mov %al, P		#store al back into processor status
	ret
	
set_overflow_1:
	mov P, %al		#store processor status in al
	or $0x40, %al		#clear overflow flag
	mov %al, P		#store al back into processor status
	ret

set_carry_0:
	mov P, %al		#store processor status in al
	and $0xFE, %al		#set overflow flag
	mov %al, P		#store al back into processor status
	ret
	
set_carry_1:
	mov P, %al		#store processor status in al
	or $0x1, %al		#clear overflow flag
	mov %al, P		#store al back into processor status
	ret



#######################################
#####frombcd: Convert a BDC to hex#####
#######################################
frombcd:
	pushl %ebp
	pushl %eax
	pushl %ebx
	pushl %edx
	movl %esp, %ebp

	movl $0, %ebx
	movl $0, %eax
	mov 20(%ebp),%eax	##read number
	
	andb $0xF0, %al
	
	shr $4, %al

	mov $0x0A, %dl	
	mul %dl

	mov 20(%ebp), %ebx

	andb $0x0F, %bl

	addb %bl, %al	
	
	mov %eax, 20(%ebp)	##adjust number
	movl %ebp, %esp
	popl %edx
	popl %ebx
	popl %eax
	popl %ebp
	ret


#####################################
#####tobcd: Convert a hex to BDC#####
#####################################
tobcd:
	pushl %ebp
	pushl %eax
	pushl %ebx
	pushl %ecx
	movl %esp, %ebp

	movl $0, %ebx
	movl $0, %eax
	movl $0x0A, %ecx	
	mov 20(%ebp), %eax

	
	div %cl

	mov %al, %bl	
	shl $4, %bl
	add %ah, %bl
	
	mov %ebx, 20(%ebp)

	movl %ebp, %esp
	popl %ecx
	popl %ebx
	popl %eax
	popl %ebp
	ret
