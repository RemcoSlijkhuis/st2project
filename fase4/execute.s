
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
	

	movl $0, %ebx			# clear ebx
	mov MEM(%ecx), %bl		# load value at specified address into bl
	
	movl $0, %eax			# clear eax
	mov A, %al			# load accumulator into dl
	mov A, %ah			# and into ah as well
	
	testb $0x08, P			# check if the decimal flag is set
	jz ADC_continue			# if it's 0, skip converting from BCD to hex

	pushl %eax			# push the current accumulator on the stack
	call frombcd			# convert the accumulator from a BCD to a normal hex number
	popl %eax			# and store the converted accumulator back into eax
	movb %al, %ah			# also copy the new accumulator into ah, so al and ah contain the same value

	pushl %ebx			# push the argument onto the stack
	call frombcd			# and convert the accumulator from a BCD to a normal hex number
	popl %ebx			# then store the converted number back in ebx

ADC_continue:				# continue here if the decimal mode flag is not set
	call load_C			# set the x86 carry flag based on the P register

	adc %bl, %al			# add argument to accumulator with carry
	pushf				# and store the current x86 processor status on the stack
	mov %al, A			# store al back in the accumulator
	
	mov $0xFF, %cx			# store maximum value for non-BCD mode before carry occurs in cx
	testb $0x08, P			# test if the decimal flag is set
	jz ADC_end2			# if not, continue at ADC_end2
	mov $99, %cx			# store maximum value for BCD mode before carry occurs in cx
ADC_end2:
	mov %ah, %al			# store the high register into the low register
	mov $0, %ah			# and clear out the old high register
	adc %bx, %ax			# add the argument to the (second instance of the) accumulator
	cmp %cx, %ax			# if the result is too high for the current mode (BCD or non-BCD)
	ja ADC_carry			# then, jump to ADC_carry to set the carry flag
	call set_carry_0		# otherwise, set the carry flag to 0
	jmp ADC_carry_end		
ADC_carry:			
	call set_carry_1		# set the carry flag
ADC_carry_end:
	
	call check_O			# store the x86 overflow flag into the P register, using the processor status on the stack
	
	push A				# push the accumulator on the stack
	call check_ZS			# properly set the zero and negative flags in the P register
	
	testb $0x08, P			# check if the decimal flag is set
	jz ADC_end			# if not, jump to the end of the function

	movl $0, %eax			# clear eax
	movb A, %al			# store the accumulator into al
	pushl %eax			# and push the accumulator in eax on the stack
	call tobcd			# convert the BCD accumulator to a normal hex value
	popl %eax			# load the result back into eax
	movb %al, A			# and store the result back in the accumulator

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

	mov MEM(%ecx), %al		# store the argument into al
	and %al, A			# add the argument to the accumulator
	push A				# push the new accumulator on the stack
	call check_ZS			# adjust zero en negative flags

	movl %ebp, %esp
	popl %ebp
	ret
	

########################################################################################
## ASL                                                                                ##
## shift bits one place to the left, LSB is set 0, affects sign and zero flags        ##
## the bit that is shifted out affects the carry                                      ##
########################################################################################
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
	decw PC 		#decrease program counter with 1

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
	decw PC 		#decrease program counter with 1

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

	decw PC

	#restore stack pointer and return
BEQ_end:
	movl %ebp, %esp
	popl %ebp
	ret
	

##checks if memory adress and accumulator are different for each bit by performing an AND operation, without changing them
##the negative flag is set equal to bit 7 of the memory value and the overflow value is set equals to bit 6 of the memory value
## the zero flag is set according to the result of the AND operation (1 if each bit is not equal)
execute_BIT:
	pushl %ebp				#Prolog
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	movzbl A, %eax				# load the accumulator for the and operation
	movzbl MEM(%ecx), %ebx			# retrieve memory value
	AND %bl, %al				# perform AND operation on accumulator and memory value
	
	andb $0xFD, P				# always clear the zero flag
	cmpb $0x0, %al				# see if the result of the and operation is zero
	jnz BIT_zero_end			# if not, skip setting the zero flag
	orb $0x02, P				# otherwise, set the zero flag

BIT_zero_end:
	
	testb $0x80, %bl			#retrieve bit 7 of memory value and set negative flag accordingly
	jnz BIT_negative
	AND $0x7F, P				#value is positive, clear negative flag
	jmp BIT_OVtest

BIT_negative:	
	orb $0x80, P				#value is negative, set negative flag

BIT_OVtest:
	call set_overflow_0			# always clear overflow flag
	testb $0x40, %bl			# test if the 6th bit is set
	jz BIT_end				# if not, skip setting the overflow flag
	call set_overflow_1			# else, set the overflow flag
BIT_end:
	
	movl %ebp, %esp				#no return or changed values
	popl %ebx				#restore registers
	popl %eax
	popl %ebp	
	ret
	

#################################################################
## BMI:                                                        ##
## checks if negative flag is set, if so branch is taken,      ##
## else execution resumes normally.                            ##
#################################################################
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
	decw PC
	#restore stack pointer and return
BMI_end:
	movl %ebp, %esp
	popl %ebp
	ret


#################################################################
## BNE:                                                        ##
## checks if zero flag is set, if not branch is taken,         ##
## else execution resumes normally.                            ##
#################################################################	
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
	decw PC

	#restore stack pointer and return
BNE_end:
	movl %ebp, %esp
	popl %ebp
	ret



	
#################################################################
## BPL:                                                        ##
## checks if negative flag is set, if not branch is taken,     ##
## else execution resumes normally.                            ##
#################################################################
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
	
	decw PC

	#restore stack pointer and return
BPL_end:
	movl %ebp, %esp
	popl %ebp
	ret


######################################################################
## BRK:                                                             ##
## runs interrupt subroutine, after pushing current SP+2 on stack,  ##
## low byte first and pushing the status register and 0x10.         ##
######################################################################
execute_BRK:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	movl $0x0100, %ebx			# set the ebx to point at the 01:00 page
	mov S, %bl				# store the stack pointer on the low byte of ebx (01:SS)
	movzwl PC, %eax				# load the program counter in ax
	addw $2, %ax				# increment the program counter with 2
	mov %al, MEM(%ebx)			# first store the lower byte on the stack
	decb %bl				# decrease the stack pointer
	mov %ah, MEM(%ebx)			# store the higher byte of the program counter on the stack
	decb %bl				# decrease the stack pointer
	
	movb P, %dl				# store the processor status in dl
	movb %dl, MEM(%ebx)			# and store it on the stack
	decb %bl				# decrease the program counter
	movb $0x10, MEM(%ebx)			# store the break flag on the stack
	decb %bl				# and decrease the stack pointer again
	
	movb %bl, S				# store the new stack pointer
	
	movl $0xFFFE, %eax			# where to look for low byte of handler address
	movzbl MEM(%eax), %ebx			# retreive low byte off handler
	movl $0xFFFF, %eax			# where to look for high byte of handler address
	movb MEM(%eax), %bh			# retreive high byte off handler
	movw %bx, PC				# store the location of the interrupt handler in the PC
	decw PC					# adjust PC, because fetch ends with incrementing PC.
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret

#################################################################
## BVC:                                                        ##
## checks if overflow flag is set, if not branch is taken,     ##
## else execution resumes normally.                            ##
#################################################################
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

	decw PC
	#restore stack pointer and return
BVC_end:
	movl %ebp, %esp
	popl %ebp
	ret

	

#################################################################
## BVS:                                                        ##
## checks if overflow flag is set, if so branch is taken,      ##
## else execution resumes normally.                            ##
#################################################################
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
	decw PC

	#restore stack pointer and return
BVS_end:
	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
## CLC: Clear carry flag                                       ##
#################################################################
execute_CLC:
	pushl %ebp
	movl %esp, %ebp
	
	mov P, %al
	and $0xFE, %al			# change the first bit to 0
	mov %al, P

	movl %ebp, %esp
	popl %ebp
	ret		

#################################################################
## CLD: Clear decimal flag                                     ##
#################################################################
execute_CLD:
	pushl %ebp
	movl %esp, %ebp

	mov P, %al			# store processor status in al
	and $0xF7, %al			# clear Decimal flag
	mov %al, P			# store al back into processor status

	movl %ebp, %esp
	popl %ebp
	ret
	
#################################################################
## CLI: Sets the interrupt disable bit to 0                    ##
#################################################################
execute_CLI:
	pushl %ebp
	movl %esp, %ebp
	
	#set bit 2 to zero
	andb $0xfb,P
	
	movl %ebp, %esp
	popl %ebp
	ret

	

#################################################################
## CLV: Clear overflow flag                                    ##
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
		

#################################################################
## CMP: Compare memory and accumulator                         ##
#################################################################
execute_CMP:
	pushl %ebp			# Prolog
	pushl %ebx
	movl %esp, %ebp	
	
	call set_carry_0		# always clear the carry flag
	movzbl A, %ebx			# Clear ebx and move the accumulator in it
	
	cmp MEM(%ecx), %ebx		# Compare ebx with zero
	jb CMP_end			# if the memory value was bigger than the accumulator, skip setting the carry flag
	call set_carry_1		# else, set the carry flag

CMP_end:
	
	subb MEM(%ecx), %bl		# Substract the operand from ebx
	
	push %ebx			# Push ebx for a check for the zero and negative flag	
	call check_ZS			# Subroutine check_ZS
	movl %ebp, %esp			# Restore the registers
	
	popl %ebx
	popl %ebp
	ret
	

#################################################################
## CPX: Compare memory and X                                   ##
#################################################################
execute_CPX:
	pushl %ebp			# Prolog
	pushl %ebx
	movl %esp, %ebp	
	
	call set_carry_0		# always clear the carry flag
	movzbl X, %ebx			# Clear ebx and move the x register in it
	
	cmp MEM(%ecx), %ebx		# Compare ebx with zero
	jb CPX_end			# if the memory value was bigger than the x register, skip setting the carry flag
	call set_carry_1		# else, set the carry flag

CPX_end:
	
	subb MEM(%ecx), %bl		# Substract the operand from ebx
	
	push %ebx			# Push ebx for a check for the zero and negative flag	
	call check_ZS			# Subroutine check_ZS
	movl %ebp, %esp			# Restore the registers
	
	popl %ebx
	popl %ebp
	ret	

#################################################################
## CPY: Compare memory and Y                                   ##
#################################################################
execute_CPY:
	pushl %ebp			# Prolog
	pushl %ebx
	movl %esp, %ebp	
	
	call set_carry_0		# always clear the carry flag
	movzbl Y, %ebx			# Clear ebx and move the y register in it
	
	cmp MEM(%ecx), %ebx		# Compare ebx with zero
	jb CPY_end			# if the memory value was bigger than the y register, skip setting the carry flag
	call set_carry_1		# else, set the carry flag

CPY_end:
	
	subb MEM(%ecx), %bl		# Substract the operand from ebx
	
	push %ebx			# Push ebx for a check for the zero and negative flag	
	call check_ZS			# Subroutine check_ZS
	movl %ebp, %esp			# Restore the registers
	
	popl %ebx
	popl %ebp
	ret			


#################################################################
## DEC: Lower mem with 1                                       ##
#################################################################
execute_DEC:
	pushl %ebp
	movl %esp, %ebp

	mov MEM(%ecx), %al
	dec %al
	mov %al, MEM(%ecx)
	
	push MEM(%ecx)
	call check_ZS			# adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
## DEX: Lower X with 1                                         ##
#################################################################
execute_DEX:
	pushl %ebp
	movl %esp, %ebp

	mov X, %al
	dec %al
	mov %al, X
	push X
	call check_ZS			# adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
## DEY: Lower Y with 1                                         ##
#################################################################
execute_DEY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al
	dec %al
	mov %al, Y
	push Y
	call check_ZS			# adjust zero en neg. flags

	movl %ebp, %esp
	popl %ebp
	ret	

#################################################################
## EOR: Exclusive or memory with accumulator                   ##
#################################################################
execute_EOR:
	pushl %ebp
	movl %esp, %ebp
	
	mov MEM(%ecx), %bl		# load argument into bl
	xor A, %bl			# xor accumulator and bl
	mov %bl, A			# move bl back to the accumulator
	
	push A
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

#################################################################
## INC: Increments memory by one                               ##
#################################################################
execute_INC:
	pushl %ebp
	movl %esp, %ebp
	
	movl $0, %ebx
	
	mov MEM(%ecx), %bl		# load old value from memory
	inc %bl				# increment with one
	mov %bl, MEM(%ecx)		# store back in memory
	
	push %ebx
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret	

#################################################################
## INX: Increments x register by one                           ##
#################################################################
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

#################################################################
## INY: Increments y register by one                           ##
#################################################################
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

#################################################################
## JMP: Jumps by setting the program counter to a new value    ##
#################################################################
execute_JMP:

	pushl %ebp
	movl %esp, %ebp
	#load new program counter
	movw %cx, PC

	decw PC
	
	movl %ebp, %esp
	popl %ebp
	ret

#################################################################
## JSR:                                                        ##
## Jumps by setting the program counter to a new value,        ##
## stores second byte after jsr instruction on stack           ##
#################################################################
execute_JSR:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	movl $0x0100, %ebx			# preload the mem pointer to 01:00
	movzwl PC, %edx 			# load the program counter
	
	movb S, %bl				# load stack pointer
	movb %dl, MEM(%ebx)			# store the low byte of the program counter
	decb %bl				# decrease the stack pointer with one
	movb %dh, MEM(%ebx)			# store the high byte of the program counter on the stack
	decb %bl				# decrease the stack pointer again
	mov %bl, S				# store the stack pointer back
	
	movw %cx, PC				# load new program counter
	decw PC					# decrease the new program counter with one to compensate for fetch
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret	

#################################################################
## LDA:                                                        ##
## load accumulator with memory,                               ##
## affects negative and zero flags                             ##
#################################################################
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



	
#################################################################
## LDX:                                                        ##
## load x register with memory,                                ##
## affects negative and zero flags                             ##
#################################################################
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



	
#################################################################
## LDY:                                                        ##
## load y register with memory,                                ##
## affects negative and zero flags                             ##
#################################################################
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
	
	mov MEM(%ecx), %bl		# load argument into bl
	or A, %bl			# or accumulator and bl
	mov %bl, A			# move bl back to the accumulator
	
	push A				# push the new accumulator onto the stack
	call check_ZS			# and set the zero and negative flags properly
	
	movl %ebp, %esp
	popl %ebp
	ret	

#push accumulator on stack
execute_PHA:
	pushl %ebp
	movl %esp, %ebp
	
	mov $0x0100,%eax		# set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al			# set the last byte of the mem pointer ax to the stack pointer value
	mov A, %bl			# store the accumulator in bl
	mov %bl, MEM(%eax)		# store bl in memory on the position indicated by mem pointer ax
	dec %al				# decrease the stack pointer with 1
	mov %al, S			# and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret

#push processor status on stack
execute_PHP:
	pushl %ebp
	movl %esp, %ebp
	
	mov $0x0100,%eax		# set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al			# set the last byte of the mem pointer ax to the stack pointer value
	mov P, %bl			# store the processor status in bl
	mov %bl, MEM(%eax)		# store bl in memory on the position indicated by mem pointer ax
	dec %al				# decrease the stack pointer with 1
	mov %al, S			# and store the stack pointer again
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull accumulator from stack
execute_PLA:
	pushl %ebp
	movl %esp, %ebp
	
	
	mov $0x0100,%eax		# set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al			# set the last byte of the mem pointer ax to the stack pointer value
	incb %al			# increase the stack pointer value with 1
	mov %al, S			# and store the stack pointer in the S register
	
	mov MEM(%eax),%bl		# get the value in memory on the position indicated by mem pointer ax
	mov %bl, A			# and store it in the accumulator
	
	movl %ebp, %esp
	popl %ebp
	ret	

#pull processor status from stack
execute_PLP:
	pushl %ebp
	movl %esp, %ebp
	
	
	mov $0x0100,%eax		# set the mem pointer ax to be 01:XX (currently 01:00)
	mov S, %al			# set the last byte of the mem pointer ax to the stack pointer value
	inc %al				# increase the stack pointer value with 1
	mov %al, S			# and store the stack pointer

	mov MEM(%eax),%bl		# store the value in memory on the position indicated by mem pointer ax in bl
	mov %bl, P			# store bl in the processor status
	
	
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
	

	
	
		
###############################################################################
## RTI:                                                                      ##
## return program control after (any)interrupt handling                      ##
## Processor status is restored from stack and PC is restored from stack     ##
###############################################################################
execute_RTI:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	call execute_PLP 		# pull processor status from stack
	
	movl $0x0100, %eax		# set eax to 01:00
	mov S, %al			# and store the stack pointer on the lowest byte of eax
	
	incb %al			# increase the stack pointer to read something from the stack
	movb MEM(%eax), %bh		# store the value on the stack at this position in the bh register
	
	incb %al			# increase the stack pointer to read the next byte from the stack
	movb MEM(%eax), %bl		# and store the value at the stack pointer in the bl register
	
	mov %bx, PC			# store the 2 byte value from the stack in the program counter
	mov %al, S			# and store the new, increased stack pointer in the S register
	decw PC				# decrease the program counter with one to compensate for the increment in fetch
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret


##########################################################
## RTS: return from subroutine, restores PC from stack  ##
##########################################################
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
	
##########################################################
## SBC: subtract memory from accumulator with borrow    ##
##########################################################
execute_SBC:
	pushl %ebp
	movl %esp, %ebp
	
	movzbl MEM(%ecx), %ebx		# load argument into bl
	movzbl A, %edx			# load accumulator into dl

	testb $0x08, P			# check if the decimal mode flag is set
	jz SBC_continue			# if not, continue at SBC_continue (ie don't convert numbers from BCD to hex)

	pushl %edx			# push the accumulator on the stack
	call frombcd			# convert it from BCD to hex
	popl %edx			# and store it back in edx

	pushl %ebx			# push the argument on the stack
	call frombcd			# convert it from BCD to hex
	popl %ebx			# and store it back in ebx

SBC_continue:
	
	call load_C			# set the x86 carry flag
	call swap_carry			# invert the carry, because x86 uses borrow instead of carry

	sbb %bl, %dl			# subtract the accumulator with the argument
	mov %dl, A			# store dl back in the accumulator
	
	call swap_carry			# invert x86 carry flag, to account for carry/borrow difference
	pushf				# push the x86 processor status on the stack
	call check_CO			# properly set carry and overflow flags based on the processor status on the stack
	
	push A				# push the accumulator on the stack
	call check_ZS			# properly set the zero and negative flags in the 6052 processor status
	
	testb $0x08, P			# check if the decimal mode flag is set
	jz SBC_end			# if not, continue at SBC_end, skipping the hex to BCD conversion
	
	call set_carry_1		# set carry flag to 1 -borrow is 0- when in BCD mode to begin with
	call set_sign_0			# set sign flag to 0 in BCD mode
	movl $100, %ecx			# maximum value for decimal before borrow occurs
	cmp %ecx, %edx			# if the accumulator is below this maximum value
	jb SBC_BCD_no_carry		# then, skip borrow handling
	subl $0x9C, %edx		# subtract 0x9C from the result to compensate for byte overflow vs decimal overflow
	call set_carry_0		# and set carry flag to 0, because a borrow occurred
SBC_BCD_no_carry:
	
	pushl %edx			# push the new accumulator on the stack
	call tobcd			# convert the result back to a BCD
	popl %eax			# get the result in eax
	movb %al, A			# and store the result in the A register
SBC_end:
	movl %ebp, %esp
	popl %ebp
	ret		

##############################################
## SEC: Set carry flag                      ##
##############################################
execute_SEC:
	pushl %ebp
	movl %esp, %ebp
	
	orb $0x01, P			# set the carry flag
	
	movl %ebp, %esp
	popl %ebp
	ret	

##############################################
## SED: Set decimal flag                    ##
##############################################
execute_SED:
	pushl %ebp
	movl %esp, %ebp

	orb $0x08, P			# set the decimal flag to 1

	movl %ebp, %esp
	popl %ebp
	ret

##############################################
## SEI: Set interrupt disable flag          ##
##############################################
execute_SEI:
	pushl %ebp
	movl %esp, %ebp
	
	orb $0x04,P			# set the interrupt disable flag to 1
	
	movl %ebp, %esp
	popl %ebp
	ret

##############################################
## STA: store accumulator in memory         ##
##############################################
execute_STA:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al			# store the accumulator in al
	mov %al, MEM(%ecx)		# store al in memory at the given address
	
	movl %ebp, %esp
	popl %ebp
	ret	
	
##############################################
## STP: stop instruction                    ##
##############################################
execute_STP:
	ret	

##############################################
## STX: store the x register into memory    ##
##############################################
execute_STX:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al			# store the x register in al
	mov %al, MEM(%ecx)		# store al in memory at the given address
	
	movl %ebp, %esp
	popl %ebp
	ret	

##############################################
## STY: store the y register into memory    ##
##############################################
execute_STY:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al			# store the y register in al
	mov %al, MEM(%ecx)		# store al in memory at the given address
	
	
	movl %ebp, %esp
	popl %ebp
	ret	

#Transfer accumulator to X register
execute_TAX:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al			# move accumulator to al register
	mov %al, X			# move al register to X
	push X				# push the new X value on the stack
	call check_ZS			# and properly set the zero and sign flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#Transfer accumulator to Y register
execute_TAY:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %al			# move accumulator to al register
	mov %al, Y			# move al register to Y
	push Y
	call check_ZS
	
	movl %ebp, %esp
	popl %ebp
	ret

#transfer stack pointer to x register
execute_TSX:
	pushl %ebp
	movl %esp, %ebp
	
	mov S, %al			# move the stack pointer to al
	mov %al, X			# move al to the x register
	push X				# push al onto the stack for check_ZS
	call check_ZS			# set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret		

#transfer x register into accumulator
execute_TXA:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al			# move the x register to al
	mov %al, A			# move al to the accumulator
	push A				# push the new value of the accumulator onto the stack
	call check_ZS			# set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret

	
#transfer x register to stack pointer
execute_TXS:
	pushl %ebp
	movl %esp, %ebp
	
	mov X, %al			# move the x register to al
	mov %al, S			# move al to the stack pointer
	push S				# push al onto the stack for check_ZS
	call check_ZS			# set negative and zero flags
	
	movl %ebp, %esp
	popl %ebp
	ret	

#transfer y register to accumulator
execute_TYA:
	pushl %ebp
	movl %esp, %ebp
	
	mov Y, %al			# move the y register to al
	mov %al, A			# move al to the accumulator
	push A				# push al onto the stack for check_ZS
	call check_ZS			# set negative and zero flags
	
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
	jz set_zero			# Declare the zero flag true, the negative flag unknown
	js set_neg			# Declare the zero flag false, the negative flag true 
	jmp set_none			# Declare the zero flag false and the negative flag false 

set_zero:
	js set_both			# set both zero flag and negative flag to true
	or $0x02, %bl			# zero flag true
	and $0x7F, %bl 			# negative flag false
	jmp set_end

set_neg:
	and $0xFD, %bl			# zero flag false
	or $0x80, %bl			# negative flag true
	jmp set_end

set_none:
	and $0xFD, %bl			# zero flag false
	and $0x7F, %bl 			# zero flag false
	jmp set_end
set_both:
	or $0x02, %bl			# zero flag true
	or $0x80, %bl			# negative flag true
	jmp set_end		
set_end:				# close the function
	mov %bl, P
	movl %ebp, %esp
	popl %ebp
	ret

swap_carry:
	jc SC_carry			# if carry is set, jump to SC_carry
	stc				# carry is not set, so set it
	jmp SC_end
	SC_carry:			# carry is set, so:
	clc				# clear carry
	SC_end:
	ret
	
	
#check for carry and overflow flags, requires processor status pushed on the stack
check_CO:
	pushl %ebp
	movl %esp, %ebp

	mov 8(%ebp),%eax
	push %eax
	popf
	
	jo CO_overflow			# if there's overflow, set overflow flag to 1
	
	#overflow flag set to 0:
	jc CO_noverflow_carry		# if x86 carry = 0, set carry to 0
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
	call set_overflow_1		# set overflow flag to 1
	call set_carry_0
	jmp CO_overflow_end
	
	CO_overflow_carry:
	call set_overflow_1		# set overflow flag to 1
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
	
	jo O_overflow			# if there's overflow, set overflow flag to 1
	
	#overflow flag set to 0:
	call set_overflow_0
	jmp O_overflow_end
	
	#overflow flag set to 1:
	O_overflow:
	call set_overflow_1		# set overflow flag to 1
	
	O_overflow_end:
	
	movl %ebp, %esp
	popl %ebp
	ret

#loads the carry flag from the 6052 processor status into the x86 flag
load_C:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	testb $0x1, P			# check to see if the carry flag is set		
	jz LoadC_nocarry		# if carry flag is 0, clear x86 carry flag
	stc				# set x86 carry flag to 1
	jmp LoadC_end
	LoadC_nocarry:			# if no carry,
	clc				# set x86 carry flag to 0
	LoadC_end:
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret

set_overflow_0:
	andb $0xBF, P			# set overflow flag
	ret
	
set_overflow_1:
	orb $0x40, P			# clear overflow flag
	ret

set_carry_0:
	andb $0xFE, P			# set overflow flag
	ret
	
set_carry_1:
	orb $0x1, P			# clear overflow flag
	ret

set_sign_0:
	andb $0x7F, P			# clear sign flag
	ret

set_sign_1:
	orb $0x80, P			# set sign flag
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
	mov 20(%ebp),%eax		# read number
	
	andb $0xF0, %al
	
	shr $4, %al

	mov $0x0A, %dl	
	mul %dl

	mov 20(%ebp), %ebx

	andb $0x0F, %bl

	addb %bl, %al	
	
	mov %eax, 20(%ebp)		# adjust number
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

	movl $0, %ebx			# clear the ebx register
	movl $0xA, %ecx			# store 0xA in the ecx register (hex equivalent of decimal 10)
	movl 20(%ebp), %eax		# store the argument into eax

	div %cl				# divide the argument with decimal 10, putting tens in al and ones in ah

	movb %ah, %bl			# store the ones-count in bl
	movb $0, %ah			# and clear the ones-count register (ah)
	shl $4, %al			# move the tens from al 4 bits to the left
	add %bl, %al			# add the ones to the shifted tens
	
	movl $0xA0, %ebx		# store the value for '100' in ebx
	divb %bl			# and divide the result by this number, storing the actual result in ah
	
	movzbl %ah, %ebx		# move the actual result to ebx
	mov %ebx, 20(%ebp)		# and place this back on the stack
	
	movl %ebp, %esp
	popl %ecx
	popl %ebx
	popl %eax
	popl %ebp
	ret
