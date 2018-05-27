bits	16
 
; Remember the memory map-- 0x500 through 0x7bff is unused above the BIOS data area.
; We are loaded at 0x500 (0x50:0)
 
org 0x500
 
jmp	main

 
%include "stdio.inc"			; basic i/o routines
%include "stdio32.inc"
%include "Gdt.inc"				; Gdt routines
%include "EnableA20.inc"
%include "Fat12.inc"			; FAT12 BIOS INTERRUPT USING FILE LOADER, USE WITH ATTENTION
 
LoadingMsg db "Preparing to load operating system...", 0x0D, 0x0A, 0x00
PModeMsg db "Entering Protected Mode...", 0x0D, 0x0A, 0x00
EnableA20Msg db "Enabled A20 with output port", 0x0D, 0x0A, 0x00
 
 
main:
 
	;   Setup segments and stack	;
 
	cli				; clear interrupts
	xor	ax, ax			; null segments
	mov	ds, ax
	mov	es, ax
	mov	ax, 0x9000		; stack begins at 0x9000-0xffff
	mov	ss, ax
	mov	sp, 0xFFFF
	sti				; we need interrupts, we need disk io
 
	;   Print loading message	;
 
	mov	si, LoadingMsg
	call	Puts16
	
	mov	si, EnableA20Msg
	call	Puts16
	
	;	Enable A20 pin
	call EnableA20WithOutputPort
	
	;   Install globak descriptor table
 
	call	InstallGDT		; install our GDT
 
	;   Go into pmode
 
	mov	si, PModeMsg
	call	Puts16
	

	mov	ebx, 0			; BX:BP points to buffer to load to
    mov	bp, IMAGE_RMODE_BASE
	mov	si, ImageName		; our file to load
	call	LoadFile		; load our file
	mov	dword [ImageSize], ecx	; save size of kernel
	cmp	ax, 0			; Test for success
	je	PrepareForStage3		; yep--onto Stage 3!
	mov	si, msgFailure		; Nope--print error
	call	Puts16
	mov	ah, 0
	int     0x16                    ; await keypress
	int     0x19                    ; warm boot computer
	cli				; If we get here, something really went wong
	hlt
	
	
PrepareForStage3:

	cli				; clear interrupts
	mov	eax, cr0		; set bit 0 in cr0--enter pmode
	or	eax, 1
	mov	cr0, eax
 
	jmp	08h:Stage3		; far jump to fix CS. Remember that the code selector is 0x8!
 
	; LAST POINT OF INTERRUOT USAGE, FINISH YOUR BUSSINES WITH INTERRUPTS BEFORE THIS POINT
 
bits 32
 
Stage3:
 

	; Set registers
 
	mov		ax, 0x10		; set data segments to data selector (0x10)
	mov		ds, ax
	mov		ss, ax
	mov		es, ax
	mov		esp, 90000h		; stack begins from 90000h
	
	call		ClearScreen32
	
	mov			ebx, mainMsg
	call		PrintString32
 
 CopyKernelToHighAddress:
  	 mov	eax, dword [ImageSize]
  	 movzx	ebx, word [bpbBytesPerSector]
  	 mul	ebx
  	 mov	ebx, 4
  	 div	ebx
   	 cld
	 
   	 mov    esi, IMAGE_RMODE_BASE
   	 mov	edi, IMAGE_PMODE_BASE
   	 mov	ecx, eax
   	 rep	movsd                   ; copy image to its protected mode address

	;	Jump kernel code

	jmp	CODE_DESC:IMAGE_PMODE_BASE; jump to our kernel! Note: This assumes Kernel's entry point is at 1 MB
 
;*******************************************************
;	Stop execution
;*******************************************************
 
STOP:
 
	cli
	hlt
	
;msg db  0x0A, 0x0A, 0x0A, "               <[ OS Development Series Tutorial 10 ]>"
;    db  0x0A, 0x0A,             "           Basic 32 bit graphics demo in Assembly Language", 0
	
mainMsg db "  _  __",0x0A
	db	" | |/ /",0x0A                            
	db	" | ' / _ __   ___  ___ ___  ___  ___ ",0x0A
	db	" |  < | '_ \ / _ \/ __/ __|/ _ \/ __|",0x0A
	db	" | . \| | | | (_) \__ \__ \ (_) \__ \",0x0A
	db	" |_|\_\_| |_|\___/|___/___/\___/|___/",0x0A,0x0A
	db	" 32 bit graphics driver supported",0x0A,0                                   