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
##########Fetch: Check each instruction##########
################################################# 
fetch:
	#start function
	pushl %ebp
	movl %esp, %ebp
	
	#primary loop
	fetchloop:
		movl $0, %eax
		mov PC, %ax			#load address of current instruction
		movl $0, %ebx		
		movb MEM(%eax), %bl
		movb %bl, IR			#load instruction from this address
		
		call showi			#show debug info
		call decode
		
		movb error,%al 
		cmp $1,%al
		je endloop
		
		mov PC, %ax
		cmp $0x8100, %ax
		je endloop
		movl $0, %ebx
		movb IR, %bl
		cmp $0xdb, %bl			
		je endloop			#stop if the current instruction is the stop instruction

		add $1, PC			#increase the program counter
		
	jmp fetchloop
	
	endloop:
	
	#end function
	movl %ebp, %esp
	popl %ebp
	ret

		## fetch operand subroutines voor verschillede adressing modes
		##
		## operands worden meegegeven in in het ecx register, oude waarden in ecx worden overschreven
		## bij de relative adressing mode wordt alleen het effectieve adres meegegeven.
		
	
	
fetch_abs:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	movl $0, %eax
	movl $0, %ebx
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%eax), %bl		#laadt low byte van operand adres	
	incl %eax					
	mov MEM(%eax),%bh		#laadt high byte van operand adres
	movl $0, %ecx
	mov MEM(%ebx), %cl		#sla operand op in cl
	
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_abX:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	movl $0, %eax
	movl $0, %ebx
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%eax), %bl		#laadt low byte van operand adres base	
	incl %eax					
	mov MEM(%eax),%bh		#laadt high byte van operand adres base
	add X, %bx 				#tel offset bij base op
	movl $0, %ecx
	mov MEM(%ebx), %cl		#sla operand op in ecx
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_abY:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	movl $0, %eax
	movl $0, %ebx
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%eax), %bl		#laadt low byte van operand adres base	
	incl %eax					
	mov MEM(%eax),%bh		#laadt high byte van operand adres base
	add Y, %bx 				#tel offset bij base op
	movl $0, %ecx
	mov MEM(%ebx), %cl		#sla operand op in ecx
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	
	ret
	
fetch_acc:
	pushl %ebp
	movl %esp, %ebp

	movl $0, %ecx	
	mov A, %cl				# laadt accumulator in ecx
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp

	movl $0, %eax	
	mov PC, %ax				# laadt PC in ax
	movl $0, %ecx
	mov MEM(%eax), %cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp


	movl $0, %eax
	movl $0, %ebx	
	mov PC, %ax				#laadt PC in ax
	mov MEM(%eax), %bl 		#laadt low byte van indirecte adres in bl
	incl %eax
	mov MEM(%eax), %bh		#laadt high byte van indirecte adres in bh
	movl $0, %eax
	mov MEM(%ebx), %al		#laadt low byte van effectieve adres in al
	incl %ebx					#ga naar high byte van effectief adres
	mov MEM(%ebx), %ah		#laadt high byte van effectieve adres in ah
	movl $0, %ecx
	mov MEM(%eax), %cl		#laadt operand in ecx
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
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


	movl $0, %eax
	mov PC, %ax				#laadt PC in ax
	movl $0, %ebx 		
	mov MEM(%eax), %bl		#laadt base adress in bl
	add X, %bl				#add offest bij base adress
	movl $0, %eax
	mov MEM(%ebx),%al		# laadt low byte van effective adress
	incl %ebx					
	mov MEM(%ebx),%ah		#laadt high byte van effective adress
	movl $0, %ecx
	mov MEM(%eax),%cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
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

	movl $0, %eax	
	mov PC, %ax				#laadt PC in ax
	movl $0, %ebx 		
	mov MEM(%eax), %bl		#laadt base adress in bl
	add X, %bl				#add offest bij base adress
	movl $0, %eax
	mov MEM(%ebx),%al		# laadt low byte van effective adress
	incl %ebx					
	mov MEM(%ebx),%ah		#laadt high byte van effective adress
	movl $0, %ecx
	mov MEM(%eax),%cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	ret
	
fetch_rel:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	movl $0, %eax
	
	mov PC, %ax				# laadt PC in ax
	movl $0, %ecx			
	mov MEM(%eax), %cl		# laadt offest in ecx
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	add PC, %cx				# tel PC bij offset op voor effectief adres
	
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_zp:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	movl $0, %eax	
	mov PC, %ax
	movl $0, %ebx			# zet ebx op 0
	mov MEM(%eax), %bl 		#laadt low byte in ebx
	mov MEM(%ebx), %cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %eax
	popl %ebx
	popl %ebp
	ret
	
fetch_zpX:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	movl $0, %eax
	movl $0, %ebx	
	mov PC, %ax
	mov MEM(%eax), %bl 		#laadt low byte in ebx
	add X,%bl				# add offset bij ebx
	mov MEM(%ebx), %cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %eax
	popl %ebx
	popl %ebp
	ret
	
fetch_zpY:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp

	movl $0, %eax
	movl $0, %ebx	
	mov PC, %ax
	mov MEM(%eax), %bl 		#laadt low byte in ebx
	add Y,%bl				# add offset bij ebx
	mov MEM(%ebx), %cl		#laadt operand in ecx
	
	incl PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %eax
	popl %ebx
	popl %ebp
	ret
	
	
	
