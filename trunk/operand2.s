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
.global fetch_r
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
		
	
	
fetch_abs:
	pushl %ebp
	pushl %eax
	pushl %ebx
	pushl %ecx
	movl %esp, %ebp
	
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%ax), %bl		#laadt low byte van operand adres	
	inc %ax					
	mov MEM(%ax),%bh		#laadt high byte van operand adres
	movl $0, %ecx
	mov MEM(%bx), %cl		#sla operand op in cl
	
	
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
	
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%ax), %bl		#laadt low byte van operand adres base	
	inc %ax					
	mov MEM(%ax),%bh		#laadt high byte van operand adres base
	add X, %bx 				#tel offset bij base op
	mov MEM(%bx), %ecx		#sla operand op in ecx
	
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
	
	mov PC, %ax 			# laadt program counter in ax
	mov MEM(%ax), %bl		#laadt low byte van operand adres base	
	inc %ax					
	mov MEM(%ax),%bh		#laadt high byte van operand adres base
	add Y, %bx 				#tel offset bij base op
	mov MEM(%bx), %ecx		#sla operand op in ecx
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	
	ret
	
fetch_acc:
	pushl %ebp
	movl %esp, %ebp
	
	mov A, %ecx				# laadt accumulator in ecx
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp
	pushl %eax
	movl %esp, %ebp
	
	mov PC, %ax				# laadt PC in ax
	mov MEM(%ax), %ecx		#laadt operand in ecx
	
	inc PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %eax
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp
	pushl %eax
	pushl %ebx
	movl %esp, %ebp
	
	mov PC, %ax				#laadt PC in ax
	mov MEM(%ax), %bl 		#laadt low byte van indirecte adres in bl
	inc %ax
	mov MEM(%ax), %bh		#laadt high byte van indirecte adres in bh
	mov MEM(%bx), %al		#laadt low byte van effectieve adres in al
	inc %bx					#ga naar high byte van effectief adres
	mov MEM(%bx), %ah		#laadt high byte van effectieve adres in ah
	mov MEM(%ax), %ecx		#laadt operand in ecx
	
	add $2, PC				#PC + 2 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebx
	popl %eax
	popl %ebp
	ret
	
fetch_inX:
	pushl %ebp
	movl %esp, %ebp
	
	mov PC, %ax				#laadt PC in ax
	movl 0, %ebx 		
	mov MEM(%ax), %bl		#laadt base adress in bl
	add X, %bl				#add offest bij base adress
	
	
	
	inc PC					#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_inY:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_r:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_zp:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_zpX:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_zpY:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
	
	
