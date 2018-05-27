%ifndef GDT_INC
%define GDT_INC
 
bits	16
 
;*******************************************
; InstallGDT()
;	- Initlaize GDT, Global Descriptor Table
;*******************************************
 
InstallGDT:
 
	cli					; clear interrupts
	pusha				; save  AX, CX, DX, BX, SP, BP, SI, DI registers
	lgdt 	[toc]		; load GDT into GDTR
	sti				; enable interrupts
	popa				; restore registers
	ret				; All done!
 
gdt_data: 
	dd 0 				; null descriptor
	dd 0 
 
; gdt code:				; code descriptor
	dw 0FFFFh 			; limit low
	dw 0 				; base low
	db 0 				; base middle
	db 10011010b 		; access
	db 11001111b 		; granularity
	db 0 				; base high
 
; gdt data:				; data descriptor
	dw 0FFFFh 			; limit low (Same as code)
	dw 0 				; base low
	db 0 				; base middle
	db 10010010b 		; access
	db 11001111b 		; granularity
	db 0				; base high
 
end_of_gdt:
toc: 
	dw end_of_gdt - gdt_data - 1 	; limit (Size of GDT)
	dd gdt_data 			; base of GDT
 
 ; give the descriptor offsets names

%define NULL_DESC 0
%define CODE_DESC 0x8
%define DATA_DESC 0x10
 
%endif ;GDT_INC