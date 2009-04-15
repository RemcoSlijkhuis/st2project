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

	MEM: .skip 65536	#Reserve 65536 bytes in the memory
	
	A:  .byte 0x00		#Create the 6502 registers
	X:  .byte 0x00
	Y:  .byte 0x00
	PC: .word 0x0000
	S:  .byte 0x00
	IR: .byte 0x00
	P:  .byte 0x00



start:
	
	movl $0xFF, S			#Initialize the Stack points

	call readprog			#Read the example program

	call initpc			#Initialize the Program counter
		
	call fetch

 	ret

initpc:
	
	movl $0, %eax			#Reset eax
	mov $0xfffd, %ax		#Move the memory location of the higher address of PC to ax
	
	movl $0, %ebx			#Reset ebx
	movb MEM(%eax), %bh		#Move the higher address of PC to bh
		
	mov $0xfffc, %ax		#Move the memory location of the lower address of PC to ax
		
	movb MEM(%eax), %bl		#Move the lower address of PC to bl

	mov %bx, PC			#Sets the PC

	ret				#Return from subroutine

