
.data

.global start	
.global MEM			#Make all variables global
.global A
.global X
.global Y
.global PC
.global S
.global IR
.global P
.global filename
.global test2



	MEM: .skip 65536	#Reserve 65536 bytes in the memory
	
	A:  .byte 0x00		#Create the 6502 registers
	X:  .byte 0x00
	Y:  .byte 0x00
	PC: .word 0x0000
	S:  .byte 0x00
	IR: .byte 0x00
	P:  .byte 0x00

.text
	test: .asciz "Exitcode: %d\n"
	test2: .asciz "Value: %x."
	cursor_hide:			# also resets color
		.byte 0x1B
		.ascii "[?25l"
		.byte 0x1B
		.ascii "[0;39;49m"
		.byte 0x1B		
		.ascii "[H"
		.byte 0x1B
		.ascii "[2J"
	cursor_show:
		.byte 0x1b
		.ascii "[?25h"
		.byte 0x1B
		.ascii "[0;39;49m"
		.byte 0x1B		
		.ascii "[H"
		.byte 0x1B
		.ascii "[2J"

##################################################
## start: Main routine                          ##
##################################################
start:
	movl %esp, %ebp

	movb $0xFF, S			# Initialize the Stack pointer
	movb $0, A			# initialise all register with value 0
	movb $0, X
	movb $0, Y
	movw $0, PC
	movw $0, IR
	movb $0, P
	
	movl $0, %eax

	memloop:			# Initialize the memory to 0
	movl $0, MEM(%eax)		# set 4 bytes to 0, at eax
	addl $4, %eax			# increase the counter with 4
	
	cmpl $65536, %eax		# while %eax <= 65536
	jne memloop			# jump back 
	
	call readprog			# read the program from a file into mem

	call initpc			# Initialize the Program counter
	
	call init_terminal		# load the proper terminal settings	

	call fetch			# start executing the program

	call restore_terminal		# restore the old terminal settings


	movl %ebp, %esp
	
	movl $0, %eax
	movb error, %al
	pushl %eax

 	call exit

########################################################
## initpc: initalise the program counter              ##
########################################################
initpc:	
	movl $0xfffd, %eax		#Move the memory location of the higher address of PC to ax	
	
	movl $0, %ebx			#clear ebx
	movb MEM(%eax), %bh		#Move the higher address of PC to bh
		
	mov $0xfffc, %ax		#Move the memory location of the lower address of PC to ax
		
	movb MEM(%eax), %bl		#Move the lower address of PC to bl

	movw %bx, PC			#Sets the PC

	ret				#Return from subroutine


########################################################
## init_terminal: sets up the terminal for normal io  ##
########################################################
init_terminal:
	lea -60(%esp), %edx		# load the data from the terminal 60 bytes before the stack pointer
	mov $0x5401, %ecx		# set ecx to 5401, get the settings
	call sys_ioctl			# perform an sys_ioctl call
	
	andb $245,-48(%esp) 		# turn off the flags for ICANON and ECHO
	
	mov $0x5403, %ecx		# set ecx to 5403, to set the new settings
	call sys_ioctl
	
	movl $cursor_hide, %ecx		# store the x86 address of the clear string in ecx
	movl $23, %edx			# store 23 in edx for the length of the string
	movl $4, %eax			# store 4 in eax, for a sys_write call
	movl $1, %ebx			# store 1 in ebx, for writing to stdout
	int $0x80			# generate a 0x80 interrupt, performing the write action
	
	ret

###########################################################
## restore_terminal: restores the old terminal settings  ##
###########################################################
restore_terminal:
	lea -60(%esp), %edx		# load the data from the terminal 60 bytes before the stack pointer
	mov $0x5401, %ecx		# set ecx to 5401, get the settings
	call sys_ioctl			# perform an sys_ioctl call
	
	orb $10,-48(%esp)		# turn on the flags for ICANON and ECHO
	
	mov $0x5403, %ecx		# set ecx to 5403, to set the new settings
	call sys_ioctl
	
	movl $cursor_show, %ecx		# store the x86 address of the clear string in ecx
	movl $23, %edx			# store 23 in edx for the length of the string
	movl $4, %eax			# store 4 in eax, for a sys_write call
	movl $1, %ebx			# store 1 in ebx, for writing to stdout
	int $0x80			# generate a 0x80 interrupt, performing the write action
	
	ret

########################################################
## sys_ioctl:                                         ##
## requires the number of the ioctl in ecx            ##
## and the address for termios struct in %edx         ##
########################################################
sys_ioctl:
	movl $54, %eax			# set eax to 54 for a sys_ioctl call
	movl $0, %ebx			# set ebx to 0 to read from stdin
	int $0x80			# interrupt to linux kernel
	ret
