.text

	!
	!  minbios - the absolute minimum necessary to support and debug boot.S
	!
	
OSSEGINIT    =  01000H     ; 010000H / 16 ; The first byte of the OS text
UART_BASEADDR=  001A0H     ; this is for comm1 on my 4 port card

.text
export minbios_WriteChar
export minbios_Red
export minbios_Blue


!
! minbios_WriteChar - writes a character to the screen.
!                     Character is held in 1 byte at bp+3
!

minbios_WriteChar:
		push	bp
		mov	bp,sp

		mov	ah, #0x03	! read cursor pos
		xor	bh,bh
		int	0x10
		xor	bx, #0x07
		mov	al, (bp+4)
		mov	ah,#14
		int	0x10
		mov	sp,bp
		pop	bp
		ret


	!
	! place a red square at the top left corner of the screen
	! 
minbios_Red:
	push   ds
	push   bx
	mov    ax, #0xb800
	mov    ds, ax
	xor    bx, bx
	mov    ax, #0x4444
	seg ds
	mov      (bx), ax     ! this is   mov  ds:(bx), ax
	seg cs
	pop    bx
	pop    ds
	ret

minbios_Blue:
	push   ds
	push   bx
	mov    ax, #0xb800
	mov    ds, ax
	xor    bx, bx
	mov    ax, #0x1144
	seg ds
	mov      (bx), ax     ! this is   mov  ds:(bx), ax
	seg cs
	pop    bx
	pop    ds
	ret
