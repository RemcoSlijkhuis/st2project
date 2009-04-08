


fetch:
	#begin functie
	pushl %ebp
	movl %esp, %ebp
	
	#hoofd loop
	fetchloop:
		movl PC, %eax			#laad adres van huidige instructie
		movl MEM(%eax),IR		#laad instructie van dit adres
		
		call showi				#toon debug data
		cmp $0xDB, IR			#vergelijk huidige instructie met de stop instructie
		jne endloop				#als dit de stop instructie is, stop dan
		addl $8, PC				#hoog de program counter op
		
	jmp fetchloop
	
	endloop:
	
	#eindig functie
	movl %ebp, %esp
	popl %ebp
	ret