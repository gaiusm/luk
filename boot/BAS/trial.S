	sub     sp, #0x20    ! enough room for the result
	mov     si, sp	     ! pointer to result
	push    ax           ! move stackptr a word below
	mov     ax, ss
	mov     ds, ax
	! yes
	! jmp     getgeoret
	mov     ah, #0x48    ! get geometry via bios
	int     #0x13
	jc      getgeoerror
	pop     ax           ! remove dummy and put sp back
	mov     si, sp	     ! copy stack pointer to result pointer reg si
	push    ax           ! replace dummy (restoring stackptr)
	mov     ax, ss
	mov     ds, ax
	mov     ax, (si+6)   ! hi word of no of cylinders
	cmp     ax, #0
	jnz     geomaxcyl
	mov     ax, (si+4)   ! lo word of no of cylinders
	mov     cx, ax

	mov     ax, (si+10)  ! hi word of no of heads
	cmp     ax, #0
	jnz     geomaxhead
	mov     ax, (si+8)   ! lo word of no of heads
	mov     bx, ax
	jmp     getgeoret
	
geomaxhead:
	mov     bx, #0xffff  ! max out the number of heads
	jmp     getgeoret

geomaxcyl:
	! more than 65536 cylinders - we max out at 0xffff
	mov     cx, #0xffff
	jmp     getgeoret
