
bits	16
org		0x7c00
start:          jmp loader

bpbOEM			db "Knossos "
bpbBytesPerSector:  	DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 1
bpbNumberOfFATs: 	    DB 2
bpbRootEntries: 	    DW 224
bpbTotalSectors: 	    DW 2880
bpbMedia: 	            DB 0xF0
bpbSectorsPerFAT: 	    DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	    DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0
bsUnused: 	            DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:	        DD 0xa0a1a2a3
bsVolumeLabel: 	        DB "MOS FLOPPY "
bsFileSystem: 	        DB "FAT12   "

msg	db	"A horse my kingdom for a horse!", 0
msg2	db	"Number of contiguous 1k memory blocks found at startup:", 0

;***************************************
;	Prints a string
;	DS=>SI: 0 terminated string
;***************************************

Print:
			lodsb					; load next byte from string from DS:SI or DS:ESI to AL
			or			al, 0		; Does AL=0?
			jz			PrintDone	; null terminator found, out
			mov			ah,	0eh		; print next
			int			10h
			jmp			Print
PrintDone:
			ret

loader:

	xor	ax, ax		; Setup segments to insure they are 0. Remember that
	mov	ds, ax		; we have ORG 0x7c00. This means all addresses are based
	mov	es, ax		; from 0x7c00:0. Because the data segments are within the same code segment, null them.

	mov	si, msg
	call	Print

	mov	si, msg2
	call	Print
	
	xor	ax, ax
	int	0x12
	mov dx,ax
	mov	ah,	0eh
	mov al,dl
	int 10h

	cli							; Clear all Interrupts
	hlt							; halt the system
	
times 510 - ($-$$) db 0			; We have to be 512 bytes. Clear the rest of the bytes with 0

dw 0xAA55