.data

filename: .asciz "image.bin"

.global readprog

readprog:
	
	pushl $MEM
	pushl $filename
	call  readimage
	addl  $-8,%esp

	ret
