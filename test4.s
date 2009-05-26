.bss
buffer: .byte 0x0
buffer2: .byte 0x0

.data
	fmt: .asciz "Buffer: %x\n"
	jFmt: .asciz "\033[0;0H"
	counter: .long 0x0
	timeout: .long 0
		 .long 0 #No delay while checking stdin
	cFmt: .asciz "\033[2J"
	counterFmt: .asciz "Counter: %d\n"



.macro sys_newselect
	xor %eax, %eax			#smaller than writing to %eax directly
	mov $142, %al			#new sys_select
	xor %ebx, %ebx
	mov $1, %bl			#highest is 0 (stdin), plus 1
	bts $0, -128(%esp)
	lea -128(%esp), %ecx		#Take some stack for the fd_set struct
	xor %edx, %edx			#writefds
	xor %esi, %esi			#exceptfds
	mov $timeout, %edi
	int $0x80
.endm

.macro sys_nanosleep length
	xor %eax, %eax
	mov $162, %al			#sys_nanosleep
	movl $1, -8(%esp)		#seconds
	movl \length, -4(%esp)		#nanoseconds
	lea -8(%esp), %ebx
	xor %ecx, %ecx			#ignore remainder
	int $0x80
.endm

#Requires the length to read in %edx
sys_read:
	xor %eax, %eax
	mov $3, %al			#sys_read
	xor %ebx, %ebx			#fd stdin
	mov $buffer, %ecx		#buffer
	int $0x80
	ret


#Requires the number of the ioctl in %ecx, address for termios struct in %edx
sys_ioctl:
	xor %eax, %eax
	mov $54, %al			#sys_ioctl
	xor %ebx, %ebx			#stdin
	int $0x80
	ret

.macro getterm
	lea -60(%esp), %edx		#big enough for a termios struct
	mov $0x5401, %ecx		#TCGETS
	call sys_ioctl 
.endm

.macro setterm				#No need to set %edx again
	mov $0x5403, %ecx		#TCSETSW
	call sys_ioctl
.endm

.global main

main:
	call execute_init_terminal
	
	call execute_cls
	
basicLoop:
	call execute_print_and_incr
	
	incl counter
	
	sys_nanosleep $100
	
	sys_newselect
	test %eax, %eax
	jz basicLoop
	
	movl $2, %edx
	call sys_read
	
	
	movzbl buffer, %eax
	pushl %eax
	pushl $fmt
	call printf
	
	jmp basicLoop
	
	call execute_restore_terminal
	
	pushl $0
  	call exit
  
 
  
execute_cls:
	pushl $cFmt
	call printf
	popl %eax
	ret
	
execute_init_terminal:
	getterm
	andb $245,-48(%esp) #c_lflag &= ~(ICANON|ECHO)
	setterm
	ret
	
execute_restore_terminal:
	getterm
	or $10,-48(%esp) #c_lflag |= (ICANON|ECHO)
	setterm
	ret
	
execute_print_and_incr:
	pushl $jFmt
	call printf
	popl %eax
	movl counter, %eax
	pushl %eax
	pushl $counterFmt
	call printf
	popl %eax
	popl %eax
	ret
