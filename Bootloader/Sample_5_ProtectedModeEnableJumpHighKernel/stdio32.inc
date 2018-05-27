%ifndef STDIO32_INC
%define STDIO32_INC

bits 32

%define	VIDMEM 0xB8000	;video buffer text mode
%define	COLUNM_NUM	80		
%define	ROW_NUM	25
%define	CHAR_ATTRIB	00000010b

_X db 0 ;we need to track last location of cursor
_Y db 0

;************************************************
;	Prints a char to X,Y on screen.
;	bl:char to print
;************************************************

PutChar32:
	pusha
	
	mov edi, VIDMEM
	
	xor eax,eax
	
	mov ecx, COLUNM_NUM*2 ;we spend 2 bytes for each byte, double bytes for columns
	mov al,byte[_Y]	;calculate current column
	mul ecx
	push eax	;store it
	
	mov al,byte[_X]	;calculate current row
	mov cl,2	;still using two bytes per char
	mul cl
	pop ecx
	add eax,ecx	;calculate relative location of char
	
	xor ecx,ecx
	add edi, eax ;calculate exact location of char
	
	cmp bl, 0x0A	;is it a new line char
	je .nextLine
	
	mov dl,bl	;next char itself
	mov dh,CHAR_ATTRIB	;char attirbute, green text black background
	mov word[edi],dx ;write mapped memory
	
	inc byte[_X]
	cmp byte[_X],COLUNM_NUM
	je .nextLine
	jmp .finish
	
	
.nextLine:
	mov byte[_X],0
	inc byte[_Y]	;beginning of the next column
	
.finish:
	popa
	ret

;************************************************
;	Prints a string at bit 32 mode whish is null terminated
;	ebx:string address
;************************************************

PrintString32:
	pusha
	mov edi,ebx

.loop:
	mov bl,byte[edi]
	cmp bl,0 ;print chars until null termination
	je .finishPrintString32
	
	call PutChar32
	inc edi
	jmp .loop

.finishPrintString32:
	popa
	ret

%endif ;STDIO32_INC

;************************************************
;	Clears screen in 32 bit mode
;************************************************

ClearScreen32:

	pusha
	
	xor cx,cx
	mov edi,VIDMEM
	
.fillNextChar:
	mov ah,CHAR_ATTRIB
	mov al, ' '
	mov word[edi],ax
	
	add edi,2
	add cx,1
	
	cmp cx, COLUNM_NUM*ROW_NUM
	jne .fillNextChar
	
	mov byte[_X],0
	mov byte[_Y],0
	call MoveCursor
	
	popa
	ret
	
	
;************************************************
;	Update the cursor position with CRT controller
;	bl: Y position
;	bh: X position
;************************************************

MoveCursor:

	pusha
	xor eax,eax
	mov ecx, COLUNM_NUM
	mov al,bh
	mul ecx ;calculate colunm offset
	add al,bh	;add row number
	
	mov	ebx, eax
	
	mov al,0x0F
	mov dx,0x03D4
	out dx,al
	
	mov al,bl
	mov dx,0x03D5
	out dx,al
	
	xor ax,ax
	mov	al, 0x0e
	mov	dx, 0x03D4
	out	dx, al
 
	mov	al, bh
	mov	dx, 0x03D5
	out	dx, al
	popa
	ret
