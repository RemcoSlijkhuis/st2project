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
	test2: .asciz "Value: %x\n"

##################################################
############## start: Main routine ###############
##################################################
start:
	movl %esp, %ebp

	movb $0xFF, S			#Initialize the Stack points
	movb $0, A
	movb $0, X
	movb $0, Y
	movw $0, PC
	movw $0, IR
	movb $0, P

	movl $0, %eax			

	memloop:			#Initialize the memory to 0
	movl $0, MEM(%eax)		
	addl $4, %eax
	
	cmpl $65536, %eax		#while %eax <= 65536
	jne memloop			#jump back 
	
	call readprog

	call initpc			#Initialize the Program counter	

	call fetch

	movl %ebp, %esp
	
	movl $0, %eax
	movb error, %al
	pushl %eax

 	call exit

##################################################
###### initpc: initalise the program counter #####
##################################################
initpc:
		
	movl $0xfffd, %eax		#Move the memory location of the higher address of PC to ax	

	movzbl MEM(%eax), %ebx		#Move the higher address of PC to bh
		
	mov $0xfffc, %ax		#Move the memory location of the lower address of PC to ax
		
	movb MEM(%eax), %bl		#Move the lower address of PC to bl

	movw %bx, PC			#Sets the PC

	ret				#Return from subroutine

