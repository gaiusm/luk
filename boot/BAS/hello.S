;	Static Name Aliases
;
	TITLE   hello.c
	.8087
_TEXT	SEGMENT  WORD PUBLIC 'CODE'
_TEXT	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP	CONST, _BSS, _DATA
	ASSUME  CS: _TEXT, DS: DGROUP, SS: DGROUP


TOPOFMEMSEG  =   08000H ;  We assume every machine has at least 512k

STACKSIZE    =    0400H ;  1k of stack space

STACKSEG     =  TOPOFMEMSEG - (STACKSIZE / 16) ; Assign the stack here


EXTRN	_WriteChar:NEAR


_TEXT      SEGMENT
	ASSUME	CS: _TEXT

	PUBLIC	_main


_main	PROC FAR

_start:	;
	; set up stack
	;
	mov	ax, STACKSEG
	mov	ss,ax
	mov	ax, 0
	mov	sp,ax

again:
	mov	ax, '1'
	push	ax
	call	_WriteChar
	pop	ax
	jmp     again

_main	ENDP


_TEXT	ENDS


END
