.bss
buffer: .byte 0x0
buffer2: .byte 0x0

.data
	plek : .long 0x0
.data
	fmt: .asciz "Buffer1: %d, Buffer2: %d\n"



#Requires the length to read in %edx
sys_read:
	xor %eax, %eax
	mov $3, %al #sys_read
	xor %ebx, %ebx #fd stdin
	mov $buffer, %ecx #buffer
	int $0x80
	ret

#Requires the number of the ioctl in %ecx, address for termios struct in %edx
sys_ioctl:
	xor %eax, %eax
	mov $54, %al #sys_ioctl
	xor %ebx, %ebx #stdin
	int $0x80
	ret

.macro getterm
	lea -60(%esp), %edx #big enough for a termios struct
	mov $0x5401, %ecx #TCGETS
	call sys_ioctl 
.endm

.macro setterm #No need to set %edx again
	mov $0x5403, %ecx #TCSETSW
	call sys_ioctl
.endm

.global main

main:
	getterm
	andb $245,-48(%esp) #c_lflag &= ~(ICANON|ECHO)
	setterm

	#pushl	$1
	#pushl	$plek
	#pushl	$0
	#call	read
	
	movl $2, %edx
	call sys_read
	movb buffer, %al
	movb %al, buffer2
	cmp $27, %al
	jne check_end
	
	movl $2, %edx
	call sys_read

check_end:
	
	getterm
	or $10,-48(%esp) #c_lflag |= (ICANON|ECHO)
	setterm
	
	movzbl buffer2, %eax
	movzbl buffer, %ebx
	pushl %eax
	pushl %ebx
	pushl $fmt
	call printf
	
	pushl $0
  call exit
