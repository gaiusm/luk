;	Static Name Aliases
;
	TITLE   bad.s
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


EXTRN	_WriteChar:NEAR
EXTRN	_KillMotor:NEAR


_TEXT      SEGMENT
	ASSUME	CS: _TEXT

	PUBLIC	_main


_main	PROC FAR

_start:	;
	call	_KillMotor

_again:
	mov	ax, 'b'
	push	ax
	call	_WriteChar
	pop	ax

	mov	ax, 'a'
	push	ax
	call	_WriteChar
	pop	ax

	mov	ax, 'd'
	push	ax
	call	_WriteChar
	pop	ax

	mov	ax, ' '
	push	ax
	call	_WriteChar
	pop	ax

	jmp	_again
_main	ENDP

_TEXT	ENDS


END
