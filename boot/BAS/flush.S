		;
		; flush the look ahead buffer
		;
		; jmp far ptr flush

		DB      0EAH
		DD      flush
		DW	8*3
flush:

	mov	ax, 'd'
	push	ax
	call	_WriteComm1
	pop	ax
