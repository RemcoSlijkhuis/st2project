.data


.global readprog



readprog:
	
	pushl %ebp
	movl %esp, %ebp
	movl $0xfffc, %ecx
	movb $0x00, MEM(%ecx)
	incl %ecx
	movb $0xfe, MEM(%ecx)

	movl $0xfe00, %ecx
	movb $0xe8, MEM(%ecx)
	incl %ecx
rpli:
	movb $0xc8, MEM(%ecx)
	incl %ecx
	cmpl $0xfe05, %ecx
	jl   rpli
	movb $0x9a, MEM(%ecx)
	incl %ecx
	movb $0xdb, MEM(%ecx)
	movl %ebp, %esp
	popl %ebp
 	ret




