.data

filename: .asciz "image.bin"

.global readprog

readprog:
	
	pushl $MEM		#push the memory 
	pushl $filename		#push the filename
	call  readimage		#read the file into the memory
	addl  $8,%esp		#restore the stack

	ret
