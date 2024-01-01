;
;	Static Name Aliases
;
	TITLE   gopro.S
	.8087
_TEXT	SEGMENT  WORD PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT	 WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP	CONST, _BSS, _DATA
	ASSUME  CS: _TEXT, DS: DGROUP, SS: DGROUP

OSSEGINIT    =  01000H     ; 010000H / 16 ; The first byte of the OS text


EXTRN	_WriteHex:NEAR
EXTRN	_WriteLn:NEAR
EXTRN	_WriteComm1:NEAR
EXTRN	_KillMotor:NEAR

;
; This module is called by second after the operating system has been
; loaded.
;
; Its purpose is to reseat the operating system at physical location
; 0000H, to reprogram the interrupt vectors and to jump to the
; first instruction of the operating system.
;
; It assumes that the operating system is <= 512k-64k in size.
;

_TEXT      SEGMENT
		ASSUME	CS: _TEXT

		PUBLIC	_CallOS

;
; CallOS - a one way function! Go protected and then
;          Jumps to virtual location 000000H
;

_CallOS		PROC NEAR


	mov	ax,8
	push	ax
	lea	ax, gdt
	push	ax
	call	_WriteHex
	add	sp,4



		cli			; no interrupts allowed !
		cld


; first we move the system to it's rightful place

;; basically we have loaded the system at 64k upwards
;; we are now going to move it from 0000 upwards.
;;     interrupts are off!
;; The interrupt vectors were set here for the bios
;;     thus we need to make sure that interrupts are off
;;     and then we need to reprogram the 8259 so that
;;     it doesn't use interrupt vectors at address 0x0.. 
;;     otherwise an interrupt will use code as data!

		push	ds

		mov	ax,00000H
		cld			; 'direction'=0, movs moves forward
do_move:
		mov	es,ax		; destination segment
		add	ax,01000H
		cmp	ax,07000H
		jz	end_move

		mov	ds,ax		; source segment
		sub	di,di
		sub	si,si
		mov 	cx,08000H
		rep movsw

		jmp	do_move

; then we load the segment descriptors
;; simple 386/486 flat mode segment descriptors
;; note that we have adjusted our DS. These
;; segments must live on even through this code
;; will be written over by the page tables.

end_move:
		;
		; Lets see what the first few instructions are
		; at 00000H
		;
		call	_WriteLn
		call	_WriteLn
		mov	ax, 4
		push	ax
		mov	ax, 0
		mov	ds, ax
		mov	bx, 0
		mov	ax, ds:WORD PTR [bx]	; bytes 0 & 1
		push	ax
		call	_WriteHex
		add	sp, 4

		mov	ax, 4
		push	ax
		mov	bx, 3
		mov	ax, ds:WORD PTR [bx]	; bytes 3 & 4
		push	ax
		call	_WriteHex
		add	sp, 4

		mov	ax, 4
		push	ax
		mov	bx, 5
		mov	ax, ds:WORD PTR [bx]	; bytes 5 & 6
		push	ax
		call	_WriteHex
		add	sp, 4

		mov	ax, 4
		push	ax
		mov	bx, 7
		mov	ax, ds:WORD PTR [bx]	; bytes 7 & 8
		push	ax
		call	_WriteHex
		add	sp, 4

		sub	si, si
		sub	di, di
		mov	ax, 09200H	; dest = 9200H
		mov	es, ax
		mov	ax, cs
		mov	ds, ax		; source
		mov	cx, 08000H
		rep movsw
		mov	ax, 09200H
		mov	ds, ax		; and reset data segment

		;
		; now jump to _load at a new code segment 9200:
		;

		DB	0EAH			; jmp  far   after
		DW      offset _after
		DB	LOW  9200H		;	         : 9200H
		DB	HIGH 9200H

_after:

	mov	ax, ':'
	push	ax
	call	_WriteComm1
	pop	ax

		; reach here ok


		;
		; now set DS to 9200
		;
		mov	ax, 9200H
		mov	ds, ax
		lea	bx, idt_48
;;		lidt	FWORD [idt_48]	; load idt with 0,0
		DB      0FH, 01H, 01FH

		lea	bx, gdt_48
;;		lgdt	FWORD [gdt_48]	; load gdt with whatever appropriate
		DB      0FH, 01H, 017H

		; reach here ok too!
	mov	ax, 'a'
	push	ax
	call	_WriteComm1
	pop	ax


; that was painless, now we enable A20
;;
;; Presumably this is some hardware port that enables
;; address lines 20..32


		call	empty_8042
		mov	al,0D1H		; command write
		out	064H,al
		call	empty_8042
		mov	al,0DFH		; A20 on
		out	060H,al
		call	empty_8042


		; and down to here as well...
	mov	ax, 'b'
	push	ax
	call	_WriteComm1
	pop	ax


; well, that went ok, I hope. Now we have to reprogram the interrupts :-(
; we put them right after the intel-reserved hardware interrupts, at
; int 0x20-0x2F. There they won't mess up anything. Sadly IBM really
; messed this up with the original PC, and they haven't been able to
; rectify it afterwards. Thus the bios puts interrupts at 0x08-0x0f,
; which is used for the internal hardware interrupts as well. We just
; have to reprogram the 8259's, and it isn't fun.

;;
;; We just follow this - it will work fine!
;;
		mov	al,011H		; initialization sequence
		out	020H,al		; send it to 8259A-1
		DW	00ebH,000ebH	; jmp $+2, jmp $+2
		out	0A0H,al		; and to 8259A-2
		DW	00ebH,000ebH

		mov	al,020H		; start of hardware int's (0x20)
		out	021H,al
		DW	00ebH,00ebH
		mov	al,028H		; start of hardware int's 2 (0x28)
		out	0A1H,al
		DW	00ebH,00ebH
		mov	al,004H		; 8259-1 is master
		out	021H,al
		DW	00ebH,00ebH
		mov	al,02H		; 8259-2 is slave
		out	0A1H,al
		DW	00ebH,00ebH
		mov	al,01H		; 8086 mode for both
		out	021H,al
		DW	00ebH,00ebH
		out	0A1H,al
		DW	00ebH,00ebH
		mov	al,0FFH		; mask off all interrupts for now
		out	0A1H,al
		DW	00ebH,00ebH
		mov	al,0FBH		; mask all irq's but irq2 which
		out	021H,al		; is cascaded

; well, that certainly wasn't fun :-(. Hopefully it works, and we don't
; need the BIOS anyway (except for the initial loading :-).
; The BIOS-routine wants lots of unnecessary data, and it's less
; "interesting" anyway. This is how REAL programmers do it.
;
; Well, now's the time to actually move into protected mode. To make
; things as simple as possible, we do no register set-up or anything,
; we let the gnu-compiled 32-bit programs do that. We just jump to
; absolute address 0x00000, in 32-bit protected mode.


		; and now down to here
	mov	ax, 'c'
	push	ax
	call	_WriteComm1
	pop	ax

		mov	ax,0001H	; protected mode (PE) bit
;;		lmsw	ax		; This is it!
		DB	0FH, 01H
		DB	0F0H


;		jmpi	0,8		; jmp offset 0 of segment 8 (cs)
; achieve this via:
		DB	0EAH		; jmp far ptr
		DW	0000H
		DW	0008H


		
_CallOS		ENDP


; This routine checks that the keyboard command queue is empty
; No timeout is used - if this hangs there is something wrong with
; the machine, and we probably couldn't proceed anyway.

empty_8042	PROC NEAR

		DW	00ebH,00ebH
		in	al,064H		; 8042 status port
		test	al,2		; is input buffer full?
		jnz	empty_8042	; yes - loop
		ret

empty_8042	ENDP


debug           PROC NEAR

		push	ds
		push	bx
		push	ax

		mov	bx, 4
		mov	ax, 0b800H
		mov	ds, ax
		mov	ax, 051ffH
		mov	[bx], ax

		push	ax
		push	bx
		pop	ds
		ret

debug		ENDP


		ALIGN   32

gdt:
		DW	0,0,0,0		; dummy
	
		DW	007FFH		; 8Mb - limit=2047 (2048*4096=8Mb)
		DW	00000H		; base address=0
		DW	09A00H		; code read/exec
		DW	000C0H		; granularity=4096, 386

		DW	007FFH		; 8Mb - limit=2047 (2048*4096=8Mb)
		DW	00000H		; base address=0
		DW	09200H		; data read/write
		DW	000C0H		; granularity=4096, 386

idt_48:
		DW	0		; idt limit=0
		DW	0,0		; idt base=0L

gdt_48:
		DW	0800H		; gdt limit=2048, 256 GDT entries
		DW	02000H+gdt
		DW	09H		; gdt base = 0X9xxxx
endoftext:		

_TEXT	ENDS




END
