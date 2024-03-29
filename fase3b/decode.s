.global	error

.data
error:  .byte 0x00	#Byte used to register errors encountered (illigal instruction)

.global do_ERROR
.global decode

.data
	opTable:	#Lookup Table for the execute functions
#0x
		.long do_BRK
		.long do_ORA_inX
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ORA_zp
		.long do_ASL_zp
		.long do_ERROR
		.long do_PHP
		.long do_ORA_imm
		.long do_ASL_acc
		.long do_ERROR
		.long do_ERROR
		.long do_ORA_abs
		.long do_ASL_abs
		.long do_ERROR
#1x
		.long do_BPL_rel
		.long do_ORA_inY
		.long do_ERROR
		.long do_ERROR
		.long do_ERROR
		.long do_ORA_zpX
		.long do_ASL_zpX
		.long do_ERROR
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


do_BRK:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_BRK
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ASL_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_ASL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_PHP:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_PHP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ASL_acc:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_acc
	call	execute_ASL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ASL_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_ASL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BPL_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BPL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ASL_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_ASL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CLC:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_CLC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ORA_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_ORA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ASL_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_ASL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_JSR_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_JSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BIT_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_BIT
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROL_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_ROL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_PLP:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_PLP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROL_acc:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_acc
	call	execute_ROL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BIT_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_BIT
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROL_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_ROL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BMI_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BMI
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROL_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_ROL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SEC:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_SEC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_AND_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_AND
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROL_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_ROL
	movl	%ebp, %esp
	popl	%ebp
	ret

do_RTI:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_RTI
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LSR_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_LSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_PHA:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_PHA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LSR_acc:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_acc
	call	execute_LSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_JMP_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_JMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LSR_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_LSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BVC_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BVC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LSR_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_LSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CLI:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_CLI
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_EOR_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_EOR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LSR_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_LSR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_RTS:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_RTS
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret


do_ADC_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROR_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_ROR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_PLA:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_PLA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROR_acc:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_acc
	call	execute_ROR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_JMP_ind:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_ind
	call	execute_JMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROR_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_ROR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BVS_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BVS
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROR_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_ROR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SEI:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_SEI
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ADC_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_ADC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_ROR_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_ROR
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STY_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_STY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STX_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_STX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_DEY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TXA:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TXA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STY_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_STY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STX_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_STX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BCC_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BCC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STY_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_STY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STX_zpY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpY
	call	execute_STX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TYA:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TYA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TXS:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TXS
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STA_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_STA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDY_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_LDY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDX_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_LDX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDY_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_LDY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDX_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_LDX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TAY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TAY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TAX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TAX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDY_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_LDY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDX_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_LDX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BCS_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BCS
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDY_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_LDY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDX_zpY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpY
	call	execute_LDX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CLV:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_CLV
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_TSX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_TSX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDY_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_LDY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDA_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_LDA
	movl	%ebp, %esp
	popl	%ebp
	ret

do_LDX_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_LDX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPY_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_CPY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPY_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_CPY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEC_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_DEC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_INY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_DEX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPY_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_CPY
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEC_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_DEC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BNE_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BNE
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEC_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_DEC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CLD:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_CLD
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_STP:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_STP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CMP_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_CMP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_DEC_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_DEC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPX_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_CPX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_inX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inX
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPX_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_CPX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INC_zp:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zp
	call	execute_INC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_INX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_imm:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_imm
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_NOP:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_NOP
	movl	%ebp, %esp
	popl	%ebp
	ret

do_CPX_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_CPX
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INC_abs:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abs
	call	execute_INC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_BEQ_rel:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_rel
	call	execute_BEQ
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_inY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_inY
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INC_zpX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_zpX
	call	execute_INC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SED:
	pushl	%ebp 
	movl	%esp, %ebp
	call	execute_SED
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_abY:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abY
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_SBC_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_SBC
	movl	%ebp, %esp
	popl	%ebp
	ret

do_INC_abX:
	pushl	%ebp 
	movl	%esp, %ebp
	call	fetch_abX
	call	execute_INC
	movl	%ebp, %esp
	popl	%ebp
	ret

.text
message: .asciz "Illegal instruction encountered. Program terminated. \n"

# Description : Print error message if illegal instruction is encountered.
do_ERROR:
		pushl %ebp		#Prolog
		movl %esp, %ebp	

		movl $1, %eax		#Move 1 to eax as error code to point out illigal instruction error
		movb %al, error		#Move the error code to global variable error
		
		push $message		#Push illigal instruction message
		call printf		#Print illigal instruction message
		
		movl %ebp, %esp		#restore stack pointer
		popl %ebp

		ret

# Description:	Use opTable to check which instruction shuold be executed, jump to there.
decode:
		pushl %ebp		#Prolog
		movl %esp, %ebp	
		
		movl $0, %ebx		#reset ebx
		movb IR, %bl		#Move instruction register to lower part of ebx
		shl $2, %ebx		#multiply ebx by 4
		
		movl opTable(%ebx), %ebx 	# load the address from the table
		call *%ebx 			# call the subroutine

		movl %ebp, %esp		#restore stack pointer
		popl %ebp

		ret
