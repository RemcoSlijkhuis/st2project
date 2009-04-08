
.global fetch

#################################################
##########Fetch: Check each instruction##########
################################################# 
fetch:
	#start function
	pushl %ebp
	movl %esp, %ebp
	
	#primary loop
	fetchloop:
		movl PC, %eax			#load address of current instruction
		movl MEM(%eax),IR		#load instruction from this address
		
		call showi			#show debug info
		cmp $0xDB, IR			
		jne endloop			#stop if the current instruction is the stop instruction
		addl $8, PC			#increase the program counter
		
	jmp fetchloop
	
	endloop:
	
	#end function
	movl %ebp, %esp
	popl %ebp
	ret