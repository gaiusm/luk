EXTRN	_OSStart:WORD
EXTRN	_OSSize:WORD
EXTRN	_SecondSize:WORD
EXTRN	_SecondData:WORD
EXTRN	_WriteChar
EXTRN	_KillMotor

init_OS_size PROC NEAR
	mov     bx, 248				; OSStart
	mov	ax, word ptr [bx]
	push	ax
	mov	bx, 250				; OSSize offset 250
	mov	ax, word ptr [bx]
	push	ax
	mov	bx, 252				; SecondData 252
	mov	ax, word ptr [bx]
	push	ax
	mov	bx, 254				; SecondSize 254
	mov	ax, word ptr [bx]
	pop	bx
	push	bx				; bx = SecondData
	push	ax				; ax = SecondSize

	mov	ax, cs
	add	ax, bx
	mov	ds, ax				; ds = start of data

	pop	ax				; ax = SecondSize
	mov	word ptr [_SecondSize], ax
	pop	ax				; ax = SecondData
	mov	word ptr [_SecondData], ax
	pop	ax				; ax = OSSize
	mov	word ptr [_OSSize], ax
	pop	ax				; ax = OSStart
	mov	word ptr [_OSStart], ax
	ret
_init_OS_size	ENDP
