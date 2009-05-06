	##checks if carry flag is set, if not branch is taken, else execution resumes normally.
execute_BCC:
	pushl %ebp
	movl %esp, %ebp
	
	#check if carry is set
	and $0x01, P
	cmp $0x0, P
	#jump to end if carry is set
	jne BCC_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BCC_end:
	movl %ebp, %esp
	popl %ebp
	ret

	##checks if carry flag is set, if so branch is taken, else execution resumes normally.
execute_BCS:
	pushl %ebp
	movl %esp, %ebp
	
	#check if carry is set
	and $0x01, P
	cmp $0x0, P
	#jump to end if carry is not set
	je BCS_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BCS_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if zero flag is set, if so branch is taken, else execution resumes normally.
execute_BEQ:
	pushl %ebp
	movl %esp, %ebp
	
	#check if zero flag is set
	and $0x02, P
	cmp $0x0, P
	#jump to end if zero flag is not set
	je BEQ_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BEQ_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if negative flag is set, if so branch is taken, else execution resumes normally.
execute_BMI:
	pushl %ebp
	movl %esp, %ebp
	
	#check if negative flag is set
	and $0x80, P
	cmp $0x0, P
	#jump to end if negative flag is not set
	je BMI_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BMI_end:
	movl %ebp, %esp
	popl %ebp
	ret
	

	
	
	
	
	##checks if zero flag is set, if not branch is taken, else execution resumes normally.
execute_BNE:	
	pushl %ebp
	movl %esp, %ebp
	
	#check if zero flag is set
	and $0x02, P
	cmp $0x0, P
	#jump to end if zero is set
	jne BNE_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BNE_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
	##checks if negative flag is set, if not branch is taken, else execution resumes normally.
execute_BPL:
	pushl %ebp
	movl %esp, %ebp
	
	#check if negative flag is set
	and $0x80, P
	cmp $0x0, P
	#jump to end if negative flag is set
	jne BPL_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BPL_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
##checks if overflow flag is set, if not branch is taken, else execution resumes normally.
execute_BVC:
	pushl %ebp
	movl %esp, %ebp
	
	#check if overflow flag is set
	and $0x40, P
	cmp $0x0, P
	#jump to end if overflow flag is set
	jne BVC_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BVC_end:
	movl %ebp, %esp
	popl %ebp
	ret
	
##checks if overflow flag is set, if so branch is taken, else execution resumes normally.
execute_BVS:
	pushl %ebp
	movl %esp, %ebp
	
	#check if overflow flag is set
	and $0x40, P
	cmp $0x0, P
	#jump to end if overflow flag is not set
	je BVS_end
	#change PC to simulate jump
	mov %cx, PC

	#restore stack pointer and return
BVS_end:
	movl %ebp, %esp
	popl %ebp
	ret


	