
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
	
	mov A, %ebx	##remember old Accumulator
	
	#add 1 to accumulator if carry
	mov P,%eax		#move the processor status to eax
	and $0x01, %eax		#get the last (least significant) bit from the processor status (carry)
	cmp $0, %eax
	jz ADC_nocarry		#if carry = 0, dont add carry
	#add carry
	add $1, A		
	mov $0x1, %dh		#store carry=1 for overflow check below
	jmp ADC_carry_end
	#don't add carry
	ADC_nocarry:
	mov $0x0, %dh		#store carry=0 for overflow check below
	ADC_carry_end:
	#end of adding carry
	
	mov MEM(%ecx), %dl
	add %dl, A	##add MEM to Accumulator
	
	#modify carry
	mov P, %eax		#store the processor status in eax
	cmp A,%ebx		#compare the original accumulator to the new one
	jg ADC_setcarry		#if the old accumulator was bigger, set carry
	jmp ADC_setcarry_not	#otherwise, clear carry
	ADC_setcarry:
	or $0x01,%eax		#set carry
	jmp ADC_setcarry_end
	ADC_setcarry_not:
	and $0xFE, %eax		#clear carry
	ADC_setcarry_end:
	mov %eax, P		#store processor status
	#end of modifying carry
	
	#modify overflow
	#%bl contains old accumulator, A
	#A contains new accumulator, N
	#%dl contains memory value, M
	#end of modifying overflow
	
	
	
	push A
	call check_ZS	##adjust zero en neg. flags
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
	shl $1,%al
	pushf
	mov %al, MEM(%ecx)	
ASL_checks:
	pushl %eax
	call check_ZS
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
	mov %cl, %al
	#shift 1 right, save flags for checks and update value
	shl $1,%al
	pushf
	mov %al, A
	jmp ASL_checks



execute_BCC:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	
	
execute_BCS:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BEQ:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BIT:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BMI:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BNE:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret		

execute_BPL:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BRK:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BVC:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_BVS:
	pushl %ebp
	movl %esp, %ebp
	
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

execute_CLD:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_CLI:
	pushl %ebp
	movl %esp, %ebp
	
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
		

execute_CMP:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_CPX:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_CPY:
	pushl %ebp
	movl %esp, %ebp
	
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
	
	mov MEM(%ecx), %bl	#load old value from memory
	inc %bl			#increment with one
	mov %bl, MEM(%ecx)	#store back in memory
	
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
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_JMP:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_JSR:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
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
	mov MEM(%ecx), %al
	#shift 1 left, save flags for checks and update value
	rcl $1, %al
	pushf
	mov %al, MEM(%ecx)	
ROL_checks:
	pushl %eax
	call check_ZS
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
	mov %cl, %al
	#shift 1 left, save flags for checks and update value
	rcl $1,%al
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
	rcr $1,%al
	pushf
	mov %al, MEM(%ecx)	
ROR_checks:
	pushl %eax
	call check_ZS
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
	mov %cl, %al
	#shift 1 right, save flags for checks and update value
	rcr $1,%al
	pushf
	mov %al, A
	jmp ROR_checks
	

	
	
		

execute_RTI:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_RTS:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

#subtract memory from accumulator with borrow
execute_SBC:
	pushl %ebp
	movl %esp, %ebp
	
	
	mov MEM(%ecx), %bl	#load argument into bl
	mov A, %dl		#load accumulator into dl
		
	mov P, %al		#move the processor status to al
	and $0x1, %al		#make al 0x1 if carry, 0x0 if no carry
	cmp $0x0, %al		
	je SBC_nocarry		#if carry flag is 0, set no carry
	clc			#clear x86 carry
	jmp SBC_end
	SBC_nocarry:		#if no carry,
	stc			#set x86 carry
	SBC_end:
	sbb %bl, %dl		#subtract dl with bl

	mov %dl, A	#store dl back in the accumulator
	
	jo SBC_overflow		#if there's overflow, set overflow flag
	call set_overflow_0	#else, clear overflow flag
	jmp SBC_overflow_end	
	SBC_overflow:
	call set_overflow_1	#set overflow flag
	SBC_overflow_end:

	

	mov P, %al		#load processor status into al
	
	jc SBC_setcarry		#set 6502 if x86 carry is set
	and $0xFE, %al		#clear carry flag
	jmp SBC_setcarryend
	SBC_setcarry:
	or $0x01, %al		#set carry flag
	SBC_setcarryend:
	
	mov %al, P		#store processor status
	
	push A
	call check_ZS
	
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

execute_SED:
	pushl %ebp
	movl %esp, %ebp
	
	movl %ebp, %esp
	popl %ebp
	ret	

execute_SEI:
	pushl %ebp
	movl %esp, %ebp
	
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
