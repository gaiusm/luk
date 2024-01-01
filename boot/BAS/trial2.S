	sub     sp, #0x40    ! enough room for the result
	mov     si, sp	     ! pointer to result
	mov     ax, ss
	mov     ds, ax
	mov     ax, #0x30
	mov     (si+0), ax ! 0x30 bytes of buffer space
	push    ax

	mov     ah, #0x48    ! get geometry via bios
	int     #0x13
	jc      getgeoerror
	pop     ax           ! remove dummy and put sp back
	mov     si, sp	     ! copy stack pointer to result pointer reg si
	push    ax           ! replace dummy (restoring stackptr)
	mov     ax, ss
	mov     ds, ax

!	mov     bx, #12
!	mov     cx, #34
!	jmp     getgeoret

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
	
getgeoerror:
	mov     bx, ax       ! error code back in heads
	xor     cx, cx       ! and cylinders = 0
getgeoret:
	! mov     bx, #32
	! mov     cx, #63
	pop     ax           ! remove the dummy word
	add     sp, #0x40    ! remove bios block result
