.global	error

.data
error:  .byte 0x00
.text
message: .asciz "Illegal instruction encountered. Program terminated. \n"

.global do_ERROR
.global decode

.data
	
	opTable:
#0x
		.long do_BRK
		.long do_ORA_inX
		.long do_PRINT_abs		#originally: do_ERROR
		.long do_GETKEY			#originally: do_ERROR
		.long do_GOTOXY			#originally: do_ERROR
		.long do_ORA_zp
		.long do_ASL_zp
		.long do_RAND_imm		#originally: do_ERROR
		.long do_PHP
		.long do_ORA_imm
		.long do_ASL_acc
		.long do_SLEEP_imm		#originally: do_ERROR
		.long do_CLS			#originally: do_ERROR
		.long do_ORA_abs
		.long do_ASL_abs
		.long do_SETCOLOR_imm		#originally: do_ERROR
#1x
		.long do_BPL_rel
		.long do_ORA_inY
		.long do_PRINT_ind		#originally: do_ERROR
		.long do_PRINT_inX		#originally: do_ERROR
		.long do_PRINT_inY		#originally: do_ERROR
		.long do_ORA_zpX
		.long do_ASL_zpX
		.long do_PRINT_abX
		.long do_CLC
		.long do_ORA_abY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ORA_abX
		.long do_ASL_abX
		.long do_ERROR
#2x
		.long do_JSR_abs
		.long do_AND_inX
		.long do_ERROR
		.long do_ERROR
		.long do_BIT_zp
		.long do_AND_zp
		.long do_ROL_zp
		.long do_ERROR
		.long do_PLP
		.long do_AND_imm
		.long do_ROL_acc
		.long do_ERROR
		.long do_BIT_abs
		.long do_AND_abs
		.long do_ROL_abs
		.long do_ERROR
#3x
		.long do_BMI_rel
		.long do_AND_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_AND_zpX
		.long do_ROL_zpX
		.long do_ERROR
		.long do_SEC
		.long do_AND_abY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_AND_abX
		.long do_ROL_abX
		.long do_ERROR
#4x
		.long do_RTI
		.long do_EOR_inX
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_EOR_zp
		.long do_LSR_zp
		.long do_ERROR
		.long do_PHA
		.long do_EOR_imm
		.long do_LSR_acc
		.long do_ERROR
		.long do_JMP_abs
		.long do_EOR_abs
		.long do_LSR_abs
		.long do_ERROR
#5x
		.long do_BVC_rel
		.long do_EOR_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_EOR_zpX
		.long do_LSR_zpX
		.long do_ERROR
		.long do_CLI
		.long do_EOR_abY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_EOR_abX
		.long do_LSR_abX
		.long do_ERROR
#6x
		.long do_RTS
		.long do_ADC_inX
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ADC_zp
		.long do_ROR_zp
		.long do_ERROR
		.long do_PLA
		.long do_ADC_imm
		.long do_ROR_acc
		.long do_ERROR
		.long do_JMP_ind
		.long do_ADC_abs
		.long do_ROR_abs
		.long do_ERROR
#7x
		.long do_BVS_rel
		.long do_ADC_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ADC_zpX
		.long do_ROR_zpX
		.long do_ERROR
		.long do_SEI
		.long do_ADC_abY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ADC_abX
		.long do_ROR_abX
		.long do_ERROR
#8x
		.long do_ERROR
		.long do_STA_inX
		.long do_ERROR
		.long do_ERROR
		.long do_STY_zp
		.long do_STA_zp
		.long do_STX_zp
		.long do_ERROR
		.long do_DEY
		.long do_ERROR
		.long do_TXA
		.long do_ERROR
		.long do_STY_abs
		.long do_STA_abs
		.long do_STX_abs
		.long do_ERROR
#9x
		.long do_BCC_rel
		.long do_STA_inY
		.long do_ERROR
		.long do_ERROR
		.long do_STY_zpX
		.long do_STA_zpX
		.long do_STX_zpY
		.long do_ERROR
		.long do_TYA
		.long do_STA_abY
		.long do_TXS
		.long do_ERROR
		.long do_ERROR
		.long do_STA_abX
		.long do_ERROR
		.long do_ERROR
#Ax
		.long do_LDY_imm
		.long do_LDA_inX
		.long do_LDX_imm
		.long do_ERROR
		.long do_LDY_zp
		.long do_LDA_zp
		.long do_LDX_zp
		.long do_ERROR
		.long do_TAY
		.long do_LDA_imm
		.long do_TAX
		.long do_ERROR
		.long do_LDY_abs
		.long do_LDA_abs
		.long do_LDX_abs
		.long do_ERROR
#Bx
		.long do_BCS_rel
		.long do_LDA_inY
		.long do_ERROR
		.long do_ERROR
		.long do_LDY_zpX
		.long do_LDA_zpX
		.long do_LDX_zpY
		.long do_ERROR
		.long do_CLV
		.long do_LDA_abY
		.long do_TSX
		.long do_ERROR
		.long do_LDY_abX
		.long do_LDA_abX
		.long do_LDX_abY
		.long do_ERROR
#Cx
		.long do_CPY_imm
		.long do_CMP_inX
		.long do_ERROR
		.long do_ERROR
		.long do_CPY_zp
		.long do_CMP_zp
		.long do_DEC_zp
		.long do_ERROR
		.long do_INY
		.long do_CMP_imm
		.long do_DEX
		.long do_ERROR
		.long do_CPY_abs
		.long do_CMP_abs
		.long do_DEC_abs
		.long do_ERROR
#Dx
		.long do_BNE_rel
		.long do_CMP_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_CMP_zpX
		.long do_DEC_zpX
		.long do_ERROR
		.long do_CLD
		.long do_CMP_abY
		.long do_ERROR
		.long do_STP
		.long do_ERROR
		.long do_CMP_abX
		.long do_DEC_abX
		.long do_ERROR
#Ex
		.long do_CPX_imm
		.long do_SBC_inX
		.long do_ERROR
		.long do_ERROR
		.long do_CPX_zp
		.long do_SBC_zp
		.long do_INC_zp
		.long do_ERROR
		.long do_INX
		.long do_SBC_imm
		.long do_NOP
		.long do_ERROR
		.long do_CPX_abs
		.long do_SBC_abs
		.long do_INC_abs
		.long do_ERROR
#Fx
		.long do_BEQ_rel
		.long do_SBC_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_SBC_zpX
		.long do_INC_zpX
		.long do_ERROR
		.long do_SED
		.long do_SBC_abY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_SBC_abX
		.long do_INC_abX
		.long do_ERROR


do_PRINT_abs:
	call fetch_abs
	call execute_PRINT
	ret

do_PRINT_ind:
	call fetch_ind
	call execute_PRINT
	ret

do_PRINT_inX:
	call fetch_inX
	call execute_PRINT
	ret

do_PRINT_inY:
	call fetch_inY
	call execute_PRINT
	ret

do_PRINT_abX:
	call fetch_abX
	call execute_PRINT
	ret

do_GETKEY:
	call execute_GETKEY
	ret

do_GOTOXY:
	call execute_GOTOXY
	ret

do_RAND_imm:
	call fetch_imm
	call execute_RAND
	ret

do_SLEEP_imm:
	call fetch_imm
	call execute_SLEEP
	ret

do_CLS:
	call execute_CLS
	ret

do_SETCOLOR_imm:
	call fetch_imm
	call execute_SETCOLOR
	ret

do_BRK:
	call	execute_BRK
	ret

do_ORA_inX:
	call	fetch_inX
	call	execute_ORA
	ret

do_ORA_zp:
	call	fetch_zp
	call	execute_ORA
	ret

do_ASL_zp:
	call	fetch_zp
	call	execute_ASL
	ret

do_PHP:
	call	execute_PHP
	ret

do_ORA_imm:
	call	fetch_imm
	call	execute_ORA
	ret

do_ASL_acc:
	call	fetch_acc
	call	execute_ASL
	ret

do_ORA_abs:
	call	fetch_abs
	call	execute_ORA
	ret

do_ASL_abs:
	call	fetch_abs
	call	execute_ASL
	ret

do_BPL_rel:
	call	fetch_rel
	call	execute_BPL
	ret

do_ORA_inY:
	call	fetch_inY
	call	execute_ORA
	ret

do_ORA_zpX:
	call	fetch_zpX
	call	execute_ORA
	ret

do_ASL_zpX:
	call	fetch_zpX
	call	execute_ASL
	ret

do_CLC:
	call	execute_CLC
	ret

do_ORA_abY:
	call	fetch_abY
	call	execute_ORA
	ret

do_ORA_abX:
	call	fetch_abX
	call	execute_ORA
	ret

do_ASL_abX:
	call	fetch_abX
	call	execute_ASL
	ret

do_JSR_abs:
	call	fetch_abs
	call	execute_JSR
	ret

do_AND_inX:
	call	fetch_inX
	call	execute_AND
	ret

do_BIT_zp:
	call	fetch_zp
	call	execute_BIT
	ret

do_AND_zp:
	call	fetch_zp
	call	execute_AND
	ret

do_ROL_zp:
	call	fetch_zp
	call	execute_ROL
	ret

do_PLP:
	call	execute_PLP
	ret

do_AND_imm:
	call	fetch_imm
	call	execute_AND
	ret

do_ROL_acc:
	call	fetch_acc
	call	execute_ROL
	ret

do_BIT_abs:
	call	fetch_abs
	call	execute_BIT
	ret

do_AND_abs:
	call	fetch_abs
	call	execute_AND
	ret

do_ROL_abs:
	call	fetch_abs
	call	execute_ROL
	ret

do_BMI_rel:
	call	fetch_rel
	call	execute_BMI
	ret

do_AND_inY:
	call	fetch_inY
	call	execute_AND
	ret

do_AND_zpX:
	call	fetch_zpX
	call	execute_AND
	ret

do_ROL_zpX:
	call	fetch_zpX
	call	execute_ROL
	ret

do_SEC:
	call	execute_SEC
	ret

do_AND_abY:
	call	fetch_abY
	call	execute_AND
	ret

do_AND_abX:
	call	fetch_abX
	call	execute_AND
	ret

do_ROL_abX:
	call	fetch_abX
	call	execute_ROL
	ret

do_RTI:
	call	execute_RTI
	ret

do_EOR_inX:
	call	fetch_inX
	call	execute_EOR
	ret

do_EOR_zp:
	call	fetch_zp
	call	execute_EOR
	ret

do_LSR_zp:
	call	fetch_zp
	call	execute_LSR
	ret

do_PHA:
	call	execute_PHA
	ret

do_EOR_imm:
	call	fetch_imm
	call	execute_EOR
	ret

do_LSR_acc:
	call	fetch_acc
	call	execute_LSR
	ret

do_JMP_abs:
	call	fetch_abs
	call	execute_JMP
	ret

do_EOR_abs:
	call	fetch_abs
	call	execute_EOR
	ret

do_LSR_abs:
	call	fetch_abs
	call	execute_LSR
	ret

do_BVC_rel:
	call	fetch_rel
	call	execute_BVC
	ret

do_EOR_inY:
	call	fetch_inY
	call	execute_EOR
	ret

do_EOR_zpX:
	call	fetch_zpX
	call	execute_EOR
	ret

do_LSR_zpX:
	call	fetch_zpX
	call	execute_LSR
	ret

do_CLI:
	call	execute_CLI
	ret

do_EOR_abY:
	call	fetch_abY
	call	execute_EOR
	ret

do_EOR_abX:
	call	fetch_abX
	call	execute_EOR
	ret

do_LSR_abX:
	call	fetch_abX
	call	execute_LSR
	ret

do_RTS:
	call	execute_RTS
	ret

do_ADC_inX:
	call	fetch_inX
	call	execute_ADC
	ret

do_ADC_zp:
	call	fetch_zp
	call	execute_ADC
	ret

do_ROR_zp:
	call	fetch_zp
	call	execute_ROR
	ret

do_PLA:
	call	execute_PLA
	ret

do_ADC_imm:
	call	fetch_imm
	call	execute_ADC
	ret

do_ROR_acc:
	call	fetch_acc
	call	execute_ROR
	ret

do_JMP_ind:
	call	fetch_ind
	call	execute_JMP
	ret

do_ADC_abs:
	call	fetch_abs
	call	execute_ADC
	ret

do_ROR_abs:
	call	fetch_abs
	call	execute_ROR
	ret

do_BVS_rel:
	call	fetch_rel
	call	execute_BVS
	ret

do_ADC_inY:
	call	fetch_inY
	call	execute_ADC
	ret

do_ADC_zpX:
	call	fetch_zpX
	call	execute_ADC
	ret

do_ROR_zpX:
	call	fetch_zpX
	call	execute_ROR
	ret

do_SEI:
	call	execute_SEI
	ret

do_ADC_abY:
	call	fetch_abY
	call	execute_ADC
	ret

do_ADC_abX:
	call	fetch_abX
	call	execute_ADC
	ret

do_ROR_abX:
	call	fetch_abX
	call	execute_ROR
	ret

do_STA_inX:
	call	fetch_inX
	call	execute_STA
	ret

do_STY_zp:
	call	fetch_zp
	call	execute_STY
	ret

do_STA_zp:
	call	fetch_zp
	call	execute_STA
	ret

do_STX_zp:
	call	fetch_zp
	call	execute_STX
	ret

do_DEY:
	call	execute_DEY
	ret

do_TXA:
	call	execute_TXA
	ret

do_STY_abs:
	call	fetch_abs
	call	execute_STY
	ret

do_STA_abs:
	call	fetch_abs
	call	execute_STA
	ret

do_STX_abs:
	call	fetch_abs
	call	execute_STX
	ret

do_BCC_rel:
	call	fetch_rel
	call	execute_BCC
	ret

do_STA_inY:
	call	fetch_inY
	call	execute_STA
	ret

do_STY_zpX:
	call	fetch_zpX
	call	execute_STY
	ret

do_STA_zpX:
	call	fetch_zpX
	call	execute_STA
	ret

do_STX_zpY:
	call	fetch_zpY
	call	execute_STX
	ret

do_TYA:
	call	execute_TYA
	ret

do_STA_abY:
	call	fetch_abY
	call	execute_STA
	ret

do_TXS:
	call	execute_TXS
	ret

do_STA_abX:
	call	fetch_abX
	call	execute_STA
	ret

do_LDY_imm:
	call	fetch_imm
	call	execute_LDY
	ret

do_LDA_inX:
	call	fetch_inX
	call	execute_LDA
	ret

do_LDX_imm:
	call	fetch_imm
	call	execute_LDX
	ret

do_LDY_zp:
	call	fetch_zp
	call	execute_LDY
	ret

do_LDA_zp:
	call	fetch_zp
	call	execute_LDA
	ret

do_LDX_zp:
	call	fetch_zp
	call	execute_LDX
	ret

do_TAY:
	call	execute_TAY
	ret

do_LDA_imm:
	call	fetch_imm
	call	execute_LDA
	ret

do_TAX:
	call	execute_TAX
	ret

do_LDY_abs:
	call	fetch_abs
	call	execute_LDY
	ret

do_LDA_abs:
	call	fetch_abs
	call	execute_LDA
	ret

do_LDX_abs:
	call	fetch_abs
	call	execute_LDX
	ret

do_BCS_rel:
	call	fetch_rel
	call	execute_BCS
	ret

do_LDA_inY:
	call	fetch_inY
	call	execute_LDA
	ret

do_LDY_zpX:
	call	fetch_zpX
	call	execute_LDY
	ret

do_LDA_zpX:
	call	fetch_zpX
	call	execute_LDA
	ret

do_LDX_zpY:
	call	fetch_zpY
	call	execute_LDX
	ret

do_CLV:
	call	execute_CLV
	ret

do_LDA_abY:
	call	fetch_abY
	call	execute_LDA
	ret

do_TSX:
	call	execute_TSX
	ret

do_LDY_abX:
	call	fetch_abX
	call	execute_LDY
	ret

do_LDA_abX:
	call	fetch_abX
	call	execute_LDA
	ret

do_LDX_abY:
	call	fetch_abY
	call	execute_LDX
	ret

do_CPY_imm:
	call	fetch_imm
	call	execute_CPY
	ret

do_CMP_inX:
	call	fetch_inX
	call	execute_CMP
	ret

do_CPY_zp:
	call	fetch_zp
	call	execute_CPY
	ret

do_CMP_zp:
	call	fetch_zp
	call	execute_CMP
	ret

do_DEC_zp:
	call	fetch_zp
	call	execute_DEC
	ret

do_INY:
	call	execute_INY
	ret

do_CMP_imm:
	call	fetch_imm
	call	execute_CMP
	ret

do_DEX:
	call	execute_DEX
	ret

do_CPY_abs:
	call	fetch_abs
	call	execute_CPY
	ret

do_CMP_abs:
	call	fetch_abs
	call	execute_CMP
	ret

do_DEC_abs:
	call	fetch_abs
	call	execute_DEC
	ret

do_BNE_rel:
	call	fetch_rel
	call	execute_BNE
	ret

do_CMP_inY:
	call	fetch_inY
	call	execute_CMP
	ret

do_CMP_zpX:
	call	fetch_zpX
	call	execute_CMP
	ret

do_DEC_zpX:
	call	fetch_zpX
	call	execute_DEC
	ret

do_CLD:
	call	execute_CLD
	ret

do_CMP_abY:
	call	fetch_abY
	call	execute_CMP
	ret

do_STP:
	call	execute_STP
	ret

do_CMP_abX:
	call	fetch_abX
	call	execute_CMP
	ret

do_DEC_abX:
	call	fetch_abX
	call	execute_DEC
	ret

do_CPX_imm:
	call	fetch_imm
	call	execute_CPX
	ret

do_SBC_inX:
	call	fetch_inX
	call	execute_SBC
	ret

do_CPX_zp:
	call	fetch_zp
	call	execute_CPX
	ret

do_SBC_zp:
	call	fetch_zp
	call	execute_SBC
	ret

do_INC_zp:
	call	fetch_zp
	call	execute_INC
	ret

do_INX:
	call	execute_INX
	ret

do_SBC_imm:
	call	fetch_imm
	call	execute_SBC
	ret

do_NOP:
	call	execute_NOP
	ret

do_CPX_abs:
	call	fetch_abs
	call	execute_CPX
	ret

do_SBC_abs:
	call	fetch_abs
	call	execute_SBC
	ret

do_INC_abs:
	call	fetch_abs
	call	execute_INC
	ret

do_BEQ_rel:
	call	fetch_rel
	call	execute_BEQ
	ret

do_SBC_inY:
	call	fetch_inY
	call	execute_SBC
	ret

do_SBC_zpX:
	call	fetch_zpX
	call	execute_SBC
	ret

do_INC_zpX:
	call	fetch_zpX
	call	execute_INC
	ret

do_SED:
	call	execute_SED
	ret

do_SBC_abY:
	call	fetch_abY
	call	execute_SBC
	ret

do_SBC_abX:
	call	fetch_abX
	call	execute_SBC
	ret

do_INC_abX:
	call	fetch_abX
	call	execute_INC
	ret



# Description : Print error message if illegal instruction is encountered.
do_ERROR:
	pushl %ebp			#Prolog
	movl %esp, %ebp	

	movl $1, %eax			#Move 1 to eax as error code to point out illigal instruction error
	movb %al, error			#Move the error code to global variable error
		
	push $message			#Push illigal instruction message
	call printf			#Print illigal instruction message
		
	movl %ebp, %esp			#restore stack pointer
	popl %ebp

	ret

# Description:	Use opTable to check which instruction shuold be executed, jump to there.
decode:
	movzbl IR, %ebx			#Move instruction register to lower part of ebx
	shl $2, %ebx			#multiply ebx by 4
		
	movl opTable(%ebx), %ebx 	# load the address from the table
	call *%ebx 			# call the subroutine

	ret
