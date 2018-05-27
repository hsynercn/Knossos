org	0x100000			; Kernel starts at 1 MB, high address

bits	32

jmp	Stage3

%include "stdio32.inc"

msg db  0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, 0x0A, " TEST KERNEL BINARY CODE EXECUTED...",0x0A, 0

Stage3:

	;-------------------------------;
	;   Set registers		;
	;-------------------------------;

	mov	ax, 0x10		; set data segments to data selector (0x10)
	mov	ds, ax
	mov	ss, ax
	mov	es, ax
	mov	esp, 90000h		; stack begins from 90000h

	;---------------------------------------;
	;   Clear screen and print success	;
	;---------------------------------------;

	;call	ClearScreen32
	mov	ebx, msg
	call	PrintString32

	;---------------------------------------;
	;   Stop execution			;
	;---------------------------------------;

	cli
	hlt



