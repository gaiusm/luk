.text

OSSEGINIT    =  01000H     ; 010000H / 16 ; The first byte of the OS text
UART_BASEADDR=  001A0H     ; this is for comm1 on my 4 port card

.data
Util_OSSize:
                DW 00
Util_OSStart:
        	DW 00
Util_SecondSize:
                DW 00
Util_SecondData:
                DW 00
Util_DebuggingWithGDB:
        	DW 00
Util_StackSize:
                DW 00
Util_DriveNo:
		DW 00

export Util_OSSize
export Util_OSStart
export Util_SecondSize
export Util_SecondData
export Util_DebuggingWithGDB
export Util_StackSize
export Util_DriveNo

.text
export Util_ResetFloppy
export Util_WriteChar
export Util_KillMotor
export Util_GetExtendedSize
export Util_SetKeyboard
export Util_GetSectorsPerTrack
export Util_ReadTrack
export Util_Get16
export Util_Put16
export Util_CursorOff
; export Util_InitComm1
; export Util_WriteComm1
export Util_GetVideoMode
export Util_init_OS_size
!export Util_Red
!export Util_Blue


!
! Util_WriteChar - writes a character to the screen.
!                   Character is held in 1 byte at bp+3
!

Util_WriteChar:
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
! This procedure turns off the floppy drive motor, so
! that we enter the kernel in a known state, and
! don't have to worry about it later.
!
Util_KillMotor:
		push	dx
		push	ax
		mov	dx,#0x3F2
		xor	al,al
		out	dx,al
		pop	ax
		pop	dx
		ret



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
Util_init_OS_size:
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
	mov	Util_SecondSize, ax
	pop	ax				; ax = SecondData
	mov	Util_SecondData, ax
	pop	ax				; ax = OSSize
	mov	Util_OSSize, ax
	pop	ax				; ax = OSStart
	mov	Util_OSStart, ax
	pop	ax				; ax = DebuggingWithGDB
	mov	Util_DebuggingWithGDB, ax
	pop	ax				; ax = StackSize
	mov	Util_StackSize, ax
	pop	ax		                ; ax = DriveNo
	mov     Util_DriveNo, ax
	ret


!
!
! reset the floppy drive
!
! PROCEDURE ResetFloppy ;
!
Util_ResetFloppy:
!	xor	dl, dl			! reset FDC
!	xor	ah, ah
!	int	0x13
	ret

!
! Get memory size (extended mem, kB)
! it returns the extended memory value in AX
!
!        PROCEDURE GetExtendedSize () : CARDINAL ;
!
Util_GetExtendedSize:
		mov	ah, #0x88
		int	0x15
		ret


!
! set the keyboard repeat rate to the max
!
Util_SetKeyboard:
		mov	ax, #0x305
		mov	bx, #0
		int	0x16
		ret


!
! get sectors per track
! PROCEDURE GetSectorsPerTrack () : CARDINAL ;
!
Util_GetSectorsPerTrack:

! It seems that there is no BIOS call to get the number of sectors.
! Guess 18 sectors if sector 18 can be read, 15 if sector 15 can be
! read. Otherwise guess 9.
!

!		xor	dx, dx			! drive 0, head 0
!		mov	cx, #0x012		! sector 18, track 0
!		mov	ax, #0x1000
!		mov	es, ax
!		mov	bx, #0x0		; address after setup (es = cs)
!		mov	ax, #0x0201		; service 2, 1 sector
!		int	0x13
!		jnc	got_sectors
!		mov	cx, #0x0f		; sector 15
!		mov	ax, #0201		; service 2, 1 sector
!		int	0x13
!		jnc	got_sectors
!		mov	cx, #0x0009
!got_sectors:	mov	ax, cx
	mov     ax, #0x12
	ret


!
! read in the track
! PROCEDURE ReadTrack (Head, Drive, Track, Sector, NoOfSectors,
!                      SegAddr, OffsetAddr) : BOOLEAN ;
!
Util_ReadTrack:
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

Util_Get16:
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

Util_Put16:
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
;Util_GetTest:
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
;Util_InitComm1:
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
;Util_WriteComm1:
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

Util_GetVideoMode:
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

Util_CursorOff:
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
	
!	!
!	! place a red square at the top left corner of the screen
!	! 
!Util_Red:
!	push   ds
!	push   bx
!	mov    ax, #0xb800
!	mov    ds, ax
!	xor    bx, bx
!	mov    ax, #0x4444
!	seg ds
!	mov      (bx), ax     ! this is   mov  ds:(bx), ax
!	seg cs
!	pop    bx
!	pop    ds
!	ret
!
!Util_Blue:
!	push   ds
!	push   bx
!	mov    ax, #0xb800
!	mov    ds, ax
!	xor    bx, bx
!	mov    ax, #0x1144
!	seg ds
!	mov      (bx), ax     ! this is   mov  ds:(bx), ax
!	seg cs
!	pop    bx
!	pop    ds
!	ret
