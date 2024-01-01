.text

OSSEGINIT    =  01000H     ; 010000H / 16 ; The first byte of the OS text
UART_BASEADDR=  001A0H     ; this is for comm1 on my 4 port card
extern minbios_Red
extern minbios_Blue

.data
bios_OSSize:
                DW 00
bios_OSStart:
        	DW 00
bios_SecondSize:
                DW 00
bios_SecondData:
                DW 00
bios_DebuggingWithGDB:
        	DW 00
bios_StackSize:
                DW 00
bios_DriveNo:
		DW 00

export bios_OSSize
export bios_OSStart
export bios_SecondSize
export bios_SecondData
export bios_DebuggingWithGDB
export bios_StackSize
export bios_DriveNo

.text
export bios_GetExtendedSize
export bios_SetKeyboard
export bios_GetGeometry
export bios_ReadTrack
export bios_Get16
export bios_Put16
export bios_CursorOff
; export bios_InitComm1
; export bios_WriteComm1
export bios_GetVideoMode
export bios_init_OS_size


!
! note that init_OS_size collects values from boot.S data segment
! and places them into second boot data segment.
! So this necitates:
!
! On entry the DS register is still pointing to the
! boot.S data segment (same as boot code segment).
!
! On leaving the DS register now points to second data seg.
!
! This is how the memory map looks after we exit this function:
!
! |-------------|   640k = sp
! | stack       |
! |             |
! |-------------|
! ~             ~
! ~             ~
! |-------------|
! | data        |
! |             |
! |-------------|   DS
! | code        |
! |             |
! |-------------|   CS = 640k-64k
!
!
	!
	! init_OS_size (DriveNo in ax)
	!
bios_init_OS_size:
	push    ax                              ; DriveNo
	mov     bx, #512-14			; Stack Size
	mov	ax, 0(bx)
	push	ax
	mov     bx, #512-12			; DebuggingWithGDB
	mov	ax, 0(bx)
	push	ax
	mov     bx, #512-10			; OSStart
	mov	ax, 0(bx)
	push	ax
	mov	bx, #512-8			; OSSize offset 512-8
	mov	ax, 0(bx)
	push	ax
	mov	bx, #512-6			; SecondData 512-6
	mov	ax, 0(bx)
	push	ax
	mov	bx, #512-4			; SecondSize 512-4
	mov	ax, 0(bx)
	pop	bx
	push	bx				; bx = SecondData
	push	ax				; ax = SecondSize

	; remember that SecondData is the start of the data in clicks

	mov	ax, bx
	mov	cx, cs
	add	ax, cx                          ; data follows code see map above
	mov	ds, ax				; ds = start of data

	;
	; top of stack is unaltered, so we pop the OS parameters
        ; and put them into seconds data
	;

	pop	ax				; ax = SecondSize
	mov	bios_SecondSize, ax
	pop	ax				; ax = SecondData
	mov	bios_SecondData, ax
	pop	ax				; ax = OSSize
	mov	bios_OSSize, ax
	pop	ax				; ax = OSStart
	mov	bios_OSStart, ax
	pop	ax				; ax = DebuggingWithGDB
	mov	bios_DebuggingWithGDB, ax
	pop	ax				; ax = StackSize
	mov	bios_StackSize, ax
	pop	ax		                ; ax = DriveNo
	mov     bios_DriveNo, ax
	ret


	!
	!  PROCEDURE GetGeometry (drive: CARDINAL ; VAR heads, sectors: CARDINAL) ;
	!
	!  drive = 4
	!  heads = pointer at 6
	!  sectors = pointer at 8
	!
	
bios_GetGeometry:
	push	bp
	mov	bp, sp

	push    ax
	mov     ax, si
	push    ax
	push    bx
	push    cx
	mov     ax, ds
	push    ax

	mov  	ax, (bp+4)   ! drive
	mov     dx, ax

	mov     ax, es
	push    ax
	mov     ax, di
	push    ax

	xor     di, di
	mov     es, di
	mov     ah, #8
	int     0x13
	jc      getgeoerror

	xor     ax, ax
	mov     al, dh
	inc     ax
	mov     bx, ax
	mov     ax, cx
	and     ax, #63
	mov     cx, ax
	jmp     getgeoret

getgeoerror:
	xor     bx, bx
	mov     bl, ah       ! error code back in heads
	xor     cx, cx       ! and cylinders = 0xffff
	dec     cx

getgeoret:
	pop     ax
	mov     es, ax
	pop     ax
	mov     di, ax

	mov     ax, (bp+6)   ! pointer to heads
	mov     si, ax
	mov     0(si), bx
	mov     ax, (bp+8)   ! pointer to sectors
	mov     si, ax
	mov     0(si), cx

	pop     cx
	pop     bx
	pop     ax
	mov     si, ax
	pop     ax

	mov	sp, bp
	pop	bp
	ret


!
! Get memory size (extended mem, kB)
! it returns the extended memory value in AX
!
!        PROCEDURE GetExtendedSize () : CARDINAL ;
!
bios_GetExtendedSize:
		mov	ah, #0x88
		int	0x15
		ret


!
! set the keyboard repeat rate to the max
!
bios_SetKeyboard:
		mov	ax, #0x305
		mov	bx, #0
		int	0x16
		ret


!
! read in the track
! PROCEDURE ReadTrack (Head, Drive, Track, Sector, NoOfSectors,
!                      SegAddr, OffsetAddr) : BOOLEAN ;
!
bios_ReadTrack:
!	Head       = 4
!	Drive      = 6
!	Track      = 8
!	Sector     = 10
!	NoOfSectors= 12
!	SegAddr    = 14
!	OffsetAddr = 16
	push	bp
	mov	bp,sp
	mov  	ax, (bp+4) ! Head
	mov     dh, al

	mov	ax, (bp+6) ! Drive
	mov	dl, al
	
	mov	ax, (bp+8) ! Track
	mov	ch, al
	
	mov	ax, (bp+10) ! Sector
	mov	cl, al
	
	mov	ax, (bp+14) ! SegAddr
	mov	es, ax
	
	mov	ax, (bp+16) ! OffsetAddr
	mov	bx, ax
	mov	ax, (bp+12) ! No of sectors

	! now set ah to indicate read sectors into buffer
	mov	ah, #2
	push    si
	push    di
	push	dx
	push	cx
	push	bx
	int	0x13
	jc	badread
	pop	bx
	pop	cx
	pop	dx
	pop     di
	pop     si
	mov	ax, #1
	mov	sp,bp
	pop	bp
	ret
badread:
	pop	bx
	pop	cx
	pop	dx
	pop     di
	pop     si
	mov	ax, #0
	mov	sp,bp
	pop	bp
	ret


!
! Get16 - returns a 16 bit word from address, seg, offset
!         Get16
!

bios_Get16:
		push     bp
		mov	 bp,sp
		push     si
		push     di
		push     ds
		push     bx
		mov      ax, (bp+4)
		mov	 ds, ax
		mov      ax, (bp+6)
		mov      bx, ax
		seg ds
		mov      ax, (bx)     ! this must be mov  ax, ds:(bx)
		seg cs
		pop      bx
		pop      ds
		pop      di
		pop      si
		mov      sp,bp
		pop      bp
		ret

!
! Put16 - puts a 16 bit word into address, seg, offset
!

bios_Put16:
		push     bp
		mov	 bp,sp
		push     si
		push     di
		push     ds
		push     bx
		mov      ax, (bp+4)
		mov	 ds, ax
		mov      ax, (bp+6)
		mov      bx, ax
		mov      ax, (bp+8)
		seg ds
		mov      (bx), ax     ! this must be mov  ds:(bx), ax
		seg cs
		pop      bx
		pop      ds
		pop      di
		pop      si
		mov      sp, bp
		pop      bp
		ret


;
;bios_GetTest:
;		push     bp
;		mov	 bp,sp
;		push     si
;		push     di
;		push     ds
;		push     bx
;
;		mov	 dx, (bp+4)  ! head 0  drive 0
;		mov      ax, (bp+6)
;		mov	 cx, ax      ! track 0  sector n
;		mov	 ax, #0x1000 ! segment 0x1000  (65k)
;		mov	 es, ax
;		mov	 bx, #0      ! offset 0
;		mov	 ax, #0x201  ! bios func 2  no of sectors 1
;
;		int	 0x13
;		jc	 bad
;		mov	 ax, #1
;		jmp	 done
;bad:
;		mov	 ax, #0
;done:
;		pop      bx
;		pop      ds
;		pop      di
;		pop      si
;		mov      sp,bp
;		pop      bp
;		ret
;
;
;!
;! set com1 to 9600 baud 8 data and 1 stop bit no parity.
;!
;
;bios_InitComm1:
;		push	ax
;		push	dx
;		!
;		! set dlab off
;		!
;		mov	dx, #UART_BASEADDR+3	! lcr
;		in	al, dx
;		and	al, #0x7f		! set dlab
;		out	dx, al
;		!
;		! clear the int enable register
;		!
;		mov	dx, #UART_BASEADDR+1	! int enable reg
;		xor	ax, ax
;		out	dx, al
;		!
;		! clear the control register
;		!
;		mov	dx, #UART_BASEADDR+3	! lcr
;		xor	ax, ax
;		out	dx, al
;		!
;		! clear the line status register
;		!
;		mov	dx, #UART_BASEADDR+5	! lsr
;		xor	ax, ax
;		out	dx, al
;		!
;		! get any data char
;		!
;		mov	dx, #UART_BASEADDR	! data
;		in	al, dx
;		!
;		! set up baud rate to 9600, 8, none, 1
;		!
;		!
;		! set dlab on
;		!
;		mov	dx, #UART_BASEADDR+3	! lcr
;		in	al, dx
;		or	al, #0x080		! set dlab
;		out	dx, al
;		!
;		! 9600  hi and low bytes
;		!
;		mov	dx, #UART_BASEADDR	! divisor
;		mov	ax, #0x0c		! 9600 Baud == 12
;		out	dx, ax
;		!
;		! set dlab off
;		!
;		mov	dx, #UART_BASEADDR+3	! lcr
;		in	al, dx
;		and	al, #0x07f		! set dlab
;		out	dx, al
;		!
;		! now set 8 bits no parity and 1 stop bit
;		!
;		mov	dx, #UART_BASEADDR+3	! lcr
;		mov	al, #0x3                 ! 8 bits No parity 1 Stop
;		out	dx, al
;		!
;		! clear modem control register
;		!
;		mov	dx, #UART_BASEADDR+4	! mcr
;		xor	ax, ax			! clear loopback
;		out	dx, ax
;		!
;		! and return
;		!
;		pop	dx
;		pop	ax
;		ret
;
;
;!
;! Write character to comm1
;!
;
;bios_WriteComm1:
;		push	bp
;		mov	bp,sp
;		push	ax
;		push	dx
;		mov	dx, #UART_BASEADDR+5
;comwait:
;		in	al, dx			! wait until we can write
;		test	al, #0x60
;		jz	comwait
;		mov	al, (bp+4)		! al = char
;		mov	dx, #UART_BASEADDR
;		out	dx, al			! and write character
;		pop	dx
;		pop	ax
;		mov     sp,bp
;		pop     bp
;		ret
;


!
! GetVideoMode - returns the video mode number obtained from the BIOS.
!

bios_GetVideoMode:
		push     bp
		mov	 bp,sp
		push     si
		push     di
		push     ds
		push     bx

		; Ask the BIOS for video card data information
	
		mov	ah, #0xf
		int	0x10
		;
		; bx contains display page
		; al contains video mode
		; ah contains window width
		;
		pop      bx
		pop      ds
		pop      di
		pop      si
		mov      sp,bp
		pop      bp
		ret

bios_CursorOff:
		push     bp
		mov	 bp,sp
		push     si
		push     di
		push     ds
		push     bx

		; Ask the BIOS for video card data information
	
		mov	ah, #0x1
		mov	cx, #0x2000
		int	0x10
		;
		; ch contains start of cursor on (0x20 out of range - off)
		; ah contains function number
		;
		pop      bx
		pop      ds
		pop      di
		pop      si
		mov      sp,bp
		pop      bp
		ret
	
