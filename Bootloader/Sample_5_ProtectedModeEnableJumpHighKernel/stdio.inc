%ifndef STDIO_INC
%define STDIO_INC
 
;************************************************
;INTERRUPT USING print procedure
;DS:SI: 0 end string
;************************************************
 
bits	16
 
Puts16:
	pusha
.Loop1:		
	mov al,byte[ds:si]	;load byte from string [DS:SI] to AL
	or	al, al			;is null
	jz	FinishPrint		;null char reached
	add si,1
	mov	ah,0eh			;magic number of 10h interrupt; required for char print
	int	10h				;fire char print interrupt
	jmp	.Loop1
	
	FinishPrint:
	popa
	ret
 
%endif ;STDIO_INC