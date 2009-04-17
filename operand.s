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
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_abX:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_abY:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_acc:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_imm:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_ind:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
	movl %ebp, %esp
	popl %ebp
	ret
	
fetch_inX:
	pushl %ebp
	movl %esp, %ebp
	
	inc PC				#PC + 1 om naar volgende opcode/operand  te wijzen
	
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
	
	
	
