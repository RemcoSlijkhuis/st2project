.global readimage
.text
#
# readimage( filename, mem )
#
#	Reads a memory image from file.
#
# 	filename:	ASCIZ filename to read
#	mem:		pointer to 6502 memory (64k block)
#
# returns EAX=0 upon success or EAX=-1 upon error
#
readimage:
	pushl	%ebp
	mov	%esp,%ebp

	# int fd
	pushl	$0

	# int origin
	pushl	$0

	# clear memory
	mov	$65536/4, %ecx
	mov	12(%ebp), %edi
clearloop:
	movl	$0, (%edi)
	addl	$4, %edi
	dec	%ecx
	jnz	clearloop

	# open( filename, O_RDONLY )
	pushl	$0
	pushl	8(%ebp)
	call	open
	addl	$8,%esp

	cmpl	$-1,%eax
	je	err

	movl	%eax,-4(%ebp)

	# read origin
	# read( fd, &origin, 2 )
	movl	%ebp,%eax
	subl	$8,%eax		# eax = ebp-8
	pushl	$2
	pushl	%eax
	pushl	-4(%ebp)
	call	read
	addl	$12,%esp

	# read data
	# read( fd, &mem+origin, 65536 )
	mov	12(%ebp),%eax
	addl	-8(%ebp),%eax
	pushl	$65536
	pushl	%eax
	pushl	-4(%ebp)
	call	read
	addl	$12,%esp

	# set PC to 80:00
	mov	12(%ebp),%eax
	movw	-8(%ebp),%bx
	movw	%bx,0xFFFC(%eax)

done:
	# close( fd )
	pushl	-4(%ebp)
	call	close
	addl	$4,%esp

	# no error: clear EAX
	xor	%eax,%eax

err:
	# if error, EAX is set to -1 earlier
	mov	%ebp,%esp
	popl	%ebp
	ret

