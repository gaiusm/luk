	! call    verify_checksum	
	
verify_checksum:
	push    ax
	push    bx
	push    cx
	push    dx
	xor     dx, dx
	mov     cx, #go32_checksum
	xor     bx, bx
_check:
	xor     ax, ax
	mov     al, 0(bx)
	add     dx, ax
	inc     bx
	dec     cx
	cmp     cx, #0
	jne     _check
	cmp     dx, go32_checksum
	jne     _check_fail

	mov	ah, #'p	 ! print 'p'
	xor	bh,bh
	int	0x10
	mov	ah, #'a	 ! print 'a'
	xor	bh,bh
	int	0x10
	mov	ah, #'s	 ! print 's'
	xor	bh,bh
	int	0x10
	mov	ah, #'s	 ! print 's'
	xor	bh,bh
	int	0x10
	jmp     _check_fin
_check_fail:
	mov	ah, #'f	 ! print 'f'
	xor	bh,bh
	int	0x10
	mov	ah, #'a	 ! print 'a'
	xor	bh,bh
	int	0x10
	mov	ah, #'i	 ! print 'i'
	xor	bh,bh
	int	0x10
	mov	ah, #'l	 ! print 'l'
	xor	bh,bh
	int	0x10
_check_fin:
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	ret
