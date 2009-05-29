.data

.global fetch
.global fetch_abs
.global fetch_abX
.global fetch_abY
.global fetch_acc
.global fetch_imm
.global fetch_ind
.global fetch_inX
.global fetch_inY
.global fetch_rel
.global fetch_zp
.global fetch_zpX
.global fetch_zpY


#################################################
########## Fetch: read and execute program.
########## Each loop fetches an instruction and sends it to decode routine.
################################################# 
fetch:
	pushl %ebp		#prolog
	movl %esp, %ebp
	
#primary loop
fetchloop:	
	movzwl PC, %eax		#load addres of current instruction

	movzbl MEM(%eax), %ebx	#Load from memory to lower part of ebx
	movb %bl, IR		#load instruction from bl in IR	
	
	#call showi		#Print out IR and PC registers 					
	call decode		#decode instruction
	#call showr		#Print out the rest of registers 	
	
	testb $1, error		#see if an illegal instruction was encountered
	jnz endloop		#If illigal instruction, then jump to endloop

	movzbl IR, %ebx		#move Instruction register to bl
	cmp $0xdb, %bl		#compare with stop instruction value
	je endloop		#If stop value, then exit loop	and go to endloop	
	
	incw PC			#no end instruction, increment PC	
	jmp fetchloop		#jump back to loop

endloop:	
	movl %ebp, %esp		#Restore stack pointer
	popl %ebp
	ret

######################################################################################################################################
## 				FETCH OPERAND SUBROUTINES FOR THE DIFFERENT ADRESSING MODES					 #####
##								 								 #####
## subroutine fetches operand, then retrieves effective adress of variable and puts it in the ecx register 			 #####
## ecx register entries wil be overwritten.											 #####
## subroutine for fetch_acc will pass on the variable itself, the highest bit of the ecx registers indicates if this is the case #####
######################################################################################################################################
	
fetch_abs:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp
	
	incw PC			#increment PC to point at 1st operand
	
	movzwl PC, %eax		#save the program counter in eax

	movzbl MEM(%eax), %ecx	#store low byte of effective adress in ecx	
	incl %eax
	movb MEM(%eax),%ch	# store high byte of effective adress in ecx
	
	incw PC			#increment PC to point at next instruction				
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_abX:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp

	incw PC			#increment PC to point at 1st operand
	movzwl PC, %eax		#save the program counter in eax

	movzbl MEM(%eax), %ecx	#store low byte of base adress in ecx	
	incl %eax					
	movb MEM(%eax),%ch	#store high byte of base adress in ecx	
	
	movzbl X, %edx		#move offset from X into dl
	addw %dx, %cx		#add offset to aquire effective adress
	 				
	incw PC			#increment PC to point at next instruction		
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_abY:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp				
	
	incw PC			#increment PC to point at 1st operand
	movzwl PC, %eax		#save the program counter in eax

	movzbl MEM(%eax), %ecx	#store low byte of base adress in ecx		
	incl %eax					
	mov MEM(%eax),%ch	#store high byte of base adress in ecx	
	
	movzbl Y, %edx		#move offset from Y into dl
	addw %dx, %cx		#add offset to aquire effective adress
	
	incw PC			#increment PC to point at next instruction			
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_acc:
	pushl %ebp		#prolog
	movl %esp, %ebp
	
	movl $0x80000000, %ecx	#set first bit of ecx to 1 to indicate accumulator
	movb A, %cl		#move value from accumulator into ecx		
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp		#prolog
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
			
	movzwl PC, %ecx		#load adress pointing to immediate (=PC) in ecx 		
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp		#prolog
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
	movzwl PC, %eax		#load PC
	
	movzbl MEM(%eax), %ebx 	#store low byte of indirect adress in ebx	
	incl %eax
	movb MEM(%eax), %bh	#store high byte of indirect adress in ebx
	
	movzbl MEM(%ebx), %ecx	#store low byte of effective adress in ecx	
	incl %ebx					
	movb MEM(%ebx), %ch	#store high byte of effective adress in ecx	
	
	incw PC			#increment PC to point at next instruction	
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_inX:
	pushl %ebp		#prolog
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
	movzwl PC, %eax		#load PC in ax

	movzbl MEM(%eax), %ebx	#load base adress in bl
	addb X, %bl		#add offest to base adress
	
	movzbl MEM(%ebx),%ecx	#store low byte of effective adress in ecx
	incl %ebx					
	movb MEM(%ebx),%ch	#store high byte of effective adress in ecx

	movl %ebp, %esp		#restore stack pointer
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_inY:
	pushl %ebp		#prolog
	pushl %eax
	pushl %ebx	
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
	movzwl PC, %eax		#load PC in ax
	
	movzbl MEM(%eax), %ebx	#load base adress in bl
	
	movzbl MEM(%ebx),%ecx	#store low byte of effective adress in ecx
	incl %ebx					
	movb MEM(%ebx),%ch	#store high byte of effective adress in ecx
	

	movzbl Y, %ebx		#load Y in bl
	addw %bx, %cx		#add Y to indirect adress
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_rel:		
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp
	
	incw PC			#increment PC to point at first argument
	movzwl PC, %eax		#load program counter into ax

	movzbl MEM(%eax), %ecx	#load offset into ecx
	
	cmp $0x80, %ecx
	jl fetch_rel_positive
	
				#if this point is reached, the offset is negative
	and $0x7F, %ecx		#set the first bit from the offset to 0
	addw PC, %cx		#add program counter to the offset to get the effective address
	subl $0x80, %ecx	#subtract ecx with 0x80 because the first bit of the offset was 1
	jmp fetch_rel_end
	
fetch_rel_positive:
				#if this point is reached, the offset is not negative
	addw PC, %cx		#add program counter to the offset to get the effective address
	
fetch_rel_end:
	incl %ecx		#add 1 to the effective address to compensate for PC being 1 byte lower than the address of next instruction
	
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_zp:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
	movzwl PC, %eax		#load program counter into ax

	movzbl MEM(%eax), %ecx 	#load low byte in ecx

	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_zpX:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand	
	movzwl PC, %eax		#load program counter into ax

	movzbl MEM(%eax), %ecx 	#load low byte in ecx
	addb X, %cl		#add offset to ecx

	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_zpY:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp

	incw PC			#increment PC to point at first operand
	movzwl PC, %eax		#load program counter into ax

	movzbl MEM(%eax), %ecx 	#load low byte in ecx
	addb Y,%cl		#add offset to ecx
	
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
	
	
