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

	pushl	$0			# int fd

	pushl	$0			# int origin

	
	mov	$65536/4, %ecx		# clear memory
	mov	12(%ebp), %edi
clearloop:
	movl	$0, (%edi)
	addl	$4, %edi
	dec	%ecx
	jnz	clearloop

	
	pushl	$0			# open( filename, O_RDONLY )
	pushl	8(%ebp)
	call	open
	addl	$8,%esp

	cmpl	$-1,%eax
	je	err

	movl	%eax,-4(%ebp)

	
	
	movl	%ebp,%eax			# read origin
	subl	$8,%eax				# eax = ebp-8
	pushl	$2
	pushl	%eax
	pushl	-4(%ebp)
	call	read				# read( fd, &origin, 2 )
	addl	$12,%esp

	
	
	mov	12(%ebp),%eax			# read data
	addl	-8(%ebp),%eax
	pushl	$65536
	pushl	%eax
	pushl	-4(%ebp)
	call	read				# read( fd, &mem+origin, 65536 )
	addl	$12,%esp

	
	mov	12(%ebp),%eax			# set PC to 80:00
	movw	-8(%ebp),%bx
	movw	%bx,0xFFFC(%eax)

done:
	
	pushl	-4(%ebp)			# close( fd )
	call	close
	addl	$4,%esp

	
	xor	%eax,%eax			# no error: clear EAX

err:
	
	mov	%ebp,%esp			# if error, EAX is set to -1 earlier
	popl	%ebp
	ret

