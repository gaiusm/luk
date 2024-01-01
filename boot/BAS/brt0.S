
.text

EXTRN	_M2_second_init
EXTRN	minbios_WriteChar
EXTRN   bios_init_OS_size
	
	PUBLIC	_main

.text

_main:
	mov	ax, cs		; note we leave ds alone
                                ; as it is used in Util_init_OS_size
	mov	ss, ax
	mov	sp, #0x8000     ; a very safe stack value

	push    dx              ; save drive number

	mov	ah, #0x03	; print 'a' - debugging aid
	xor	bh,bh
	int	0x10
	xor	bx, #0x07
	mov	al, #'a
	mov	ah, #14
	int	0x10

	pop     ax             	; restore drive number
	
	; now get four variables from the bootsector and
        ; after this our ds register will have been set up
        ; for the Modula-2, but not the ss.

	call	bios_init_OS_size

	; now we set ss to ds and away we go into Modula-2
	; The compiler always assumes ss=ds

	mov	ax, ds
	mov	ss, ax
	mov	sp, #0x8000
	
	; debug using Modula-2 routines now

	mov	ax, #'b
	push	ax
	call	minbios_WriteChar
	pop	ax

	;	call	init_floppy_params

	call	_M2_second_init
	jmpi	0,0x1000

	! we should never return - just to make sure..
_again:
	jmp	_again

