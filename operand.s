


fetch:
	#begin functie
	pushl %ebp
	movl %esp, %ebp
	
	#hoofd loop
	fetchloop:
		movl PC, %eax			#load address of current instruction
		movl MEM(%eax),IR		#load instruction from this address
		
		call showi				#show debug info
		cmp $0xDB, IR			
		jne endloop				#stop if the current instruction is the stop instruction
		addl $8, PC				#increase the program counter
		
	jmp fetchloop
	
	endloop:
	
	#eindig functie
	movl %ebp, %esp
	popl %ebp
	ret