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

	

 	ret



