%ifndef STDIO32_INC
%define STDIO32_INC

bits 32

%define	VIDMEM 0xB8000	;video buffer text mode
%define	COLUNM_NUM	80		
%define	ROW_NUM	25
%define	CHAR_ATTRIB	63

_X db 0
_Y db 0

;************************************************
;	Prints a char to X,Y on screen.
;	bl:char to print
;************************************************

PutChar32:
	pusha
	
	mov edi, VIDMEM
	
	xor eax,eax
	
	mov ecx, COLUNM_NUM*2
	mov al,byte[_Y]
	mul ecx
	push ecx
	
	mov al,byte[_X]
	mov cl,2
	mul cl
	pop ecx
	add eax,ecx
	
	xor ecx,ecx
	add edi, eax
	
	cmp bl, 0x0A
	je .nextLine
	
	xor ecx,ecx
	mov cl,bl
	mov ch,CHAR_ATTRIB
	mov word[edi],cx
	
.nextChar:
	inc byte[_X]
	cmp byte[_X],COLUNM_NUM
	je .nextLine
	jmp .finish
	
	
.nextLine:
	mov byte[_X],0
	inc byte[_Y]
	
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
	cmp bl,0
	je .finishPrintString32
	
	call PutChar32
	inc edi
	jmp .loop

.finishPrintString32:
	popa
	ret

%endif ;STDIO32_INC