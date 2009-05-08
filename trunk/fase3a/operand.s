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
	pushl %ebp		#Prolog	
	movl %esp, %ebp	

#primary loop	
fetchloop:	
	movl $0, %eax		#Reset eax
	mov PC, %ax		#load addres of current instruction

	movl $0, %ebx		#reset ebx	
	movb MEM(%eax), %bl	#Load from memory to lower part of ebx
	movb %bl, IR		#load instruction from bl in IR	
	
	call showi		#Print out IR and PC registers 					
	call decode		#decode instruction
	call showr		#Print out the rest of registers 	
	
	movb error,%al 		#move error to al
	cmp $1,%al		#compare error with illigal instruction value
	je endloop		#If illigal instruction, then jump to endloop

	movl $0, %ebx		#reset ebx	
	movb IR, %bl		#move Instruction register to bl
	cmp $0xdb, %bl		#compare with stop instruction value
	je endloop		#If stop value, then exit loop	and go to endloop	
	
	incl PC			#no end instruction, increment PC	
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
	
	incl PC			#increment PC to point at 1st operand
	movl $0, %eax		#reset eax
	movl $0, %ecx		#reset ecx

	mov PC, %ax 		#get Program Counter

	mov MEM(%eax), %cl	#store low byte of effective adress in ecx	
	incl %eax					
	mov MEM(%eax),%ch	#store high byte of effective adress in ecx
	
	incl PC			#increment PC to point at next instruction				
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_abX:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp

	incl PC			#increment PC to point at 1st operand
	movl $0, %eax		#reset eax
	movl $0, %ecx		#reset ecx

	mov PC, %ax 		#get Program Counter

	mov MEM(%eax), %cl	#store low byte of base adress in ecx		
	incl %eax					
	mov MEM(%eax),%ch	#store high byte of base adress in ecx	
	
	movl $0, %edx		#reset edx
	mov X, %dl		#move offset from X into dl
	add %dx, %cx		#add offset to aquire effective adress
	 				
	incl PC			#increment PC to point at next instruction		
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_abY:
	pushl %ebp		#prolog
	pushl %eax
	movl %esp, %ebp				
	
	incl PC			#increment PC to point at 1st operand
	movl $0, %eax		#reset eax
	movl $0, %ecx		#reset ecx

	mov PC, %ax 		#get Program Counter

	mov MEM(%eax), %cl	#store low byte of base adress in ecx		
	incl %eax					
	mov MEM(%eax),%ch	#store high byte of base adress in ecx	
	
	movl $0, %edx		#reset edx
	mov Y, %dl		#move offset from Y into dl
	add %dx, %cx		#add offset to aquire effective adress
	
	incl PC			#increment PC to point at next instruction			
	movl %ebp, %esp		#restore stack pointer
	popl %eax
	popl %ebp
	ret
	
fetch_acc:
	pushl %ebp		#prolog
	movl %esp, %ebp
	
	movl $0x80000000, %ecx	#set first bit of ecx to 1 to indicate accumulator
	mov A, %cl		#move value from accumulator into ecx		
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp		#prolog
	movl %esp, %ebp

	incl PC			#increment PC to point at first operand
			
	movl $0, %ecx		#reset ecx
	mov PC, %cx		#load adress pointing to immediate (=PC) in ecx 		
	
	movl %ebp, %esp		#restore stack pointer
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp		#prolog
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	incl PC			#increment PC to point at first operand
	movl $0, %eax		#reset eax
	movl $0, %ebx		#reset ebx
	movl $0, %ecx		#reset ecx

	mov PC, %ax		#load PC

	#load low and high byte off indirect adrees in bx
	mov MEM(%eax), %bl 		
	incl %eax
	mov MEM(%eax), %bh
	
	#load low and high byte off effective adress in ecx
	mov MEM(%ebx), %cl		
	incl %ebx					
	mov MEM(%ebx), %ch		
	
	#increment PC to point at next instruction
	incl PC				
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_inX:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl $0, %eax
	movl $0, %ecx
	mov PC, %ax				#laadt PC in ax
	movl $0, %ebx 		
	mov MEM(%eax), %bl		#laadt base adress in bl
	add X, %bl				#add offest bij base adress
	
	mov MEM(%ebx),%cl		# laadt low byte van effective adress
	incl %ebx					
	mov MEM(%ebx),%ch		#laadt high byte van effective adress

	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_inY:

	pushl %ebp
	pushl %eax
	pushl %ebx	
	movl %esp, %ebp

	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl $0, %eax
	movl $0, %ecx	
	mov PC, %ax				#laadt PC in ax
	movl $0, %ebx 		
	mov MEM(%eax), %bl		#laadt base adress in bl
	
	mov MEM(%ebx),%cl		# laadt low byte van effective adress
	incl %ebx					
	mov MEM(%ebx),%ch		#laadt high byte van effective adress
	
	movl $0, %ebx
	mov Y, %bl
	add %bx, %cx
	
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_rel:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	incl PC					#PC + 1 to point to the first argument
		
	movl $0, %eax				#first clear eax
	
	mov PC, %ax				#load program counter into ax
	movl $0, %ecx				#clear ecx
	mov MEM(%eax), %cl			#load offset into ecx
	
	cmp $0x80, %ecx
	jl fetch_rel_positive
	#if this point is reached, the offset is negative
	and $0x7F, %ecx				#set the first bit from the offset to 0
	add PC, %cx				#add program counter to the offset to get the effective address
	subl $0x80, %ecx			#subtract ecx with 0x80 because the first bit of the offset was 1
	jmp fetch_rel_end
	
	fetch_rel_positive:
	#if this point is reached, the offset is not negative
	add PC, %cx				#add program counter to the offset to get the effective address
	
	fetch_rel_end:
	

	incl %ecx				#add 1 to the effective address to compensate for the program counter being 1 byte lower than the address of the next instruction
	
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_zp:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp

	incl PC				#PC + 1 om naar volgende opcode/operand  te wijzen	

	movl $0, %eax	
	mov PC, %ax
	movl $0, %ecx			# zet ecx op 0
	mov MEM(%eax), %cl 		#laadt low byte in ecx

				
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_zpX:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp

	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen	
	
	movl $0, %eax
	movl $0, %ecx	
	mov PC, %ax
	mov MEM(%eax), %cl 		#laadt low byte in ecx
	add X,%cl				# add offset bij ecx

	
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_zpY:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp

	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl $0, %eax
	movl $0, %ecx	
	mov PC, %ax
	mov MEM(%eax), %cl 		#laadt low byte in ecx
	add Y,%cl				# add offset bij ecx
	
	
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
	
	
