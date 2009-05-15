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
	#start function
	pushl %ebp
	movl %esp, %ebp
	
	#primary loop
fetchloop:	
	#load addres of current instruction
	movl $0, %eax
	mov PC, %ax	
	#load instruction in IR
	movl $0, %ebx		
	movb MEM(%eax), %bl
	movb %bl, IR			
	#decode instruction en show debug info	
	call showi							
	call decode
	call showr
	#check for errors during decode session
	movb error,%al 
	cmp $1,%al
	je endloop
	#check for stop instruction, if so exit loop	
	movl $0, %ebx
	movb IR, %bl
	cmp $0xdb, %bl			
	je endloop				
	#no end instruction, increment PC and jump back to loop	
	incw PC
	jmp fetchloop	
endloop:	
	#end function
	movl %ebp, %esp
	popl %ebp
	ret

		## fetch operand subroutines for different adressing modes
		##
		## subroutine fetches operand, then retrieves effective adress of variable and puts it in the ecx register 
		## ecx register entries wil be overwritten.
		## subroutine for fetch_acc will pass on the variable itself, the highest bit of the ecx registers indicates if this is the case.
		
	
	
fetch_abs:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	#increment PC to point at 1st operand
	incw PC
	movl $0, %eax
	movl $0, %ecx
	#get PC
	mov PC, %ax 			
	#store low and high byte off effective adress in cx
	mov MEM(%eax), %cl		
	incl %eax					
	mov MEM(%eax),%ch
	
	#increment PC to point at next instruction and return
	incw PC						
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_abX:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	#increment PC to point at 1st operand
	incw PC	
	movl $0, %eax
	movl $0, %ecx
	# get PC
	mov PC, %ax 
	#load low and high byte off base addres and add offset to aquire effective addres
	mov MEM(%eax), %cl			
	incl %eax					
	mov MEM(%eax),%ch		
	
	movl $0, %edx
	mov X, %dl
	add %dx, %cx
	 				
	#increment PC to point at next instruction and return
	incw PC						
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_abY:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp				
	
	#increment PC to point at 1st operand
	incw PC	
	movl $0, %eax
	movl $0, %ecx
	# get PC
	mov PC, %ax 
	#load low and high byte off base addres and add offset to aquire effective addres
	mov MEM(%eax), %cl			
	incl %eax					
	mov MEM(%eax),%ch		
	
	movl $0, %edx
	mov Y, %dl
	add %dx, %cx
	
	
	
	
	#increment PC to point at next instruction and return
	incw PC						
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_acc:
	pushl %ebp
	movl %esp, %ebp
	#load accumulator in ecx
	movl $0x80000000, %ecx	
	mov A, %cl				
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp
	movl %esp, %ebp

	#increment PC to point at first operand
	incw PC					
	#load adress pointing to immediate (=PC) in ecx 
	movl $0, %ecx	
	mov PC, %cx				
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	incw PC
	movl $0, %eax
	movl $0, %ebx
	movl $0, %ecx	
	#load PC 
	mov PC, %ax	
	#load low and high byte off indirect adrees in bx
	mov MEM(%eax), %bl 		
	incl %eax
	mov MEM(%eax), %bh
	#load low and high byte off effective adress in ecx
	mov MEM(%ebx), %cl		
	incl %ebx					
	mov MEM(%ebx), %ch		
	#increment PC to point at next instruction
	incw PC				
	
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

	incw PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
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

	incw PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
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
	
	incw PC					#PC + 1 to point to the first argument
		
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

	incw PC				#PC + 1 om naar volgende opcode/operand  te wijzen	

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

	incw PC					#PC + 1 om naar volgende opcode/operand  te wijzen	
	
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

	incw PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl $0, %eax
	movl $0, %ecx	
	mov PC, %ax
	mov MEM(%eax), %cl 		#laadt low byte in ecx
	add Y,%cl				# add offset bij ecx
	
	
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
	
	
