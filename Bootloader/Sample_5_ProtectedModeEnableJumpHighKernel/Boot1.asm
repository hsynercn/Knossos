
;ATTENTION: THIS FILE'S COMPILED CODE HAS ONE SECTOR LIMIT

bits	16
org	0
start:	jmp	main

;BIOS parameter block
bpbOEM					db "Knossos "			; 8 bytes
bpbBytesPerSector:  	dw 512
bpbSectorsPerCluster: 	db 1
bpbReservedSectors: 	dw 1	;not in use
bpbNumberOfFATs: 		db 2	;we have two, one for corruption backup
bpbRootEntries: 		dw 224	;max count of distinc file records
bpbTotalSectors: 		dw 2880	; 2880*512 1.44 MB disk limit
bpbMedia: 				db 0xf8
bpbSectorsPerFAT: 		dw 9	;FAT table size
bpbSectorsPerTrack: 	dw 18	;every line/track has 18 sectors
bpbHeadsPerCylinder: 	dw 2	;we have two sides 2 heads
bpbHiddenSectors: 		dd 0
bpbTotalSectorsBig:     dd 0
bsDriveNumber: 	        db 0
bsUnused: 				db 0
bsExtBootSignature: 	db 0x29
bsSerialNumber:	        dd 0xa0a1a2a3	;don't care
bsVolumeLabel: 	        db "MOS FLOPPY "
bsFileSystem: 	        db "FAT12   "


;************************************************
;	Interrupt based string printing procedure' do not use out of this file, stage one usage only
;	Print a string
;	DS to SI: String starting address, prints until 0(null)
;************************************************
Print:
	mov al,byte[ds:si]	;load byte from string [DS:SI] to AL
	or	al, al			;is null
	jz	FinishPrint		;null char reached
	add si,1
	mov	ah,0eh			;magic number of 10h interrupt; required for char print
	int	10h				;fire char print interrupt
	jmp	Print
	FinishPrint:
	ret

;************************************************
; Sector read procedure for stage two loading, we need convert LBA to CHS to locate and use INT 13
; CX:Number of sectors to read
; AX:Starting linear sector
; ES:BX:Target buffer
;************************************************

ReadSectors:
    .RETYR
		mov	di, 0x0006                  ;retry number limit
	.SECTORLOOP
        push	ax
        push	bx
        push	cx
        call	ConvertLBAtoCHS         ;convert starting sector to CHS
        mov	ah, 0x02                	;BIOS read sector specific parameter
        mov al, 0x01                    ;read one sector
        mov ch, BYTE [absoluteTrack]
        mov cl, BYTE [absoluteSector]
        mov dh, BYTE [absoluteHead]
        mov dl, BYTE [bsDriveNumber]    ;CONSTANT, defined at BIOS parameter block
        int 0x13                        ;start interrupt
        jnc .SUCCESS                    ;test for read error
        xor ax, ax                      ;BIOS reset disk, we need to reset head
        int 0x13                        ;invoke BIOS
        dec di                          ;decrement error counter
        pop cx
        pop bx
        pop ax
		jnz .SECTORLOOP ;retry again
        int 0x18		;Boot fault routine
    .SUCCESS
        mov si, msgProgress	;print "-" for every sector read
        call	Print
        pop cx
        pop bx
        pop ax
        add bx,WORD[bpbBytesPerSector]        ; queue next buffer
        inc ax                                  ; queue next sector
        loop	.RETYR                               ; read next sector
        ret
;************************************************;
; Convert CHS to LBA
; LBA = (cluster - 2) * sectors per cluster
;************************************************;

ClusterLBA:
        sub	ax,0x0002                          ; zero base cluster number
        xor	cx,cx
        mov cl,BYTE[bpbSectorsPerCluster]     ; convert byte to word
        mul cx
        add ax,WORD[datasector]               ; base data sector
        ret
     
;************************************************;
;Logical Block Addressing to Cylinder/Head/Sector Addressing
;AX:Address to convert
;absolute sector = (logical sector / sectors per track) + 1
;absolute head   = (logical sector / sectors per track) MOD number of heads
;absolute track  = logical sector / (sectors per track * number of heads)
;************************************************;

ConvertLBAtoCHS:	
        xor	dx,dx                              ; prepare dx:ax for operation
        div WORD[bpbSectorsPerTrack]           ; calculate
        inc dl                                  ; adjust for sector 0
        mov BYTE[absoluteSector],dl
        xor dx,dx                              ; prepare dx:ax for operation
        div WORD[bpbHeadsPerCylinder]          ; calculate
        mov BYTE[absoluteHead],dl
        mov BYTE[absoluteTrack],al
        ret
		
;MAIN ENTRY

main:

    ;----------------------------------------------------
    ;0000:7C00, bootsector specific
    ;----------------------------------------------------
     
        cli		;clear interrupts
        mov     ax,0x07C0		;init registers
        mov     ds,ax
        mov     es,ax
        mov     fs,ax
        mov     gs,ax
     
        mov     ax,0x0000		;set the stack
		mov     ss,ax
        mov     sp,0xFFFF
        sti						;restore interrupts
        mov     si,msgStart
        call    Print

    LOAD_ROOT:
     
    ;size of root directory to cx
     
        xor	cx,cx
        xor dx,dx
        mov ax,0x0020                           ; 32 byte directory entry
        mul WORD[bpbRootEntries]                ; total size of directory, 224
        div WORD[bpbBytesPerSector]             ; sectors used by directory, 512 
        mov	cx,ax								;size of the directory tabl is 14, store it for ReadSectors call
          
    ;root directory to ax
     
        mov	al,BYTE [bpbNumberOfFATs]     	;we have 2 FATs
        mul WORD[bpbSectorsPerFAT]         	;FAT sector size, 9 currently
        add ax,WORD [bpbReservedSectors]   	;add one reserved sector
        mov WORD[datasector],ax         	;base of root directory
        add WORD[datasector],cx
          
    ; read root directory into memory (7C00:0200)
     
        mov     bx, 0x0200  	;copy root dir above bootcode, hex of 512 = 0x0200
        call    ReadSectors
		
    ; Find KRNLDR SYS on directory table

    ; browse root directory for binary image
        mov	cx,WORD[bpbRootEntries]	;load loop counter, 224 entries
        mov di, 0x0200	;locate first root entry, WE COPIED ROOT ENTY TABLE TO THIS ADDRESS
    .LOOP:
        push    cx
        mov cx, 0x000B      ;file name limit 11
        mov si, ImageName 	;image name to find
        push	di
		rep  cmpsb   	;compare strings,CMPSB compares the byte at [DS:SI] or [DS:ESI] with the byte at [ES:DI] or [ES:EDI], and sets the flags accordingly
        pop di
        je  LOAD_FAT ;found file entry
        pop cx
        add di, 0x0020	;go next 32th byte
        loop    .LOOP
        mov si, msgCantFindFile
        call    Print
        cli
		hlt

    LOAD_FAT:
     
    ;boot images first cluster to cluster
     
        mov     si, msgCRLF
        call    Print
        mov     dx,WORD[di + 0x001A] ;di is still at the root entry fisrt byte, byte26:First Logical Cluster of file, length 2 bytes
        mov     WORD[cluster],dx	;file's first cluster
          
    ;size of FAT and store in "cx"
     
        xor     ax,ax
        mov     al,BYTE[bpbNumberOfFATs]	;number of FATs, 2
        mul     WORD[bpbSectorsPerFAT]     ;sectors used by FATs, 9
        mov     cx,ax

    ; compute location of FAT and store in "ax"

        mov     ax, WORD [bpbReservedSectors]       ; adjust for bootsector
          
    ; read FAT into memory (7C00:0200)

        mov     bx, 0x0200                          ; copy FAT above bootcode
		;CX:Number of sectors to read
		;AX:Starting linear sector
		;ES:BX:Target buffer
        call    ReadSectors

    ; read image file into memory (0050:0000)
     
        mov si, msgCRLF
        call    Print
		
        mov ax,0x0050
        mov	es,ax     	;destination for KRNLDR.SYS image
        mov	bx,0x0000  ;destination for KRNLDR.SYS image
        push	bx

    LOAD_IMAGE:
     
        mov	ax,WORD[cluster]     	;cluster to read
        pop bx                      ;buffer to read into
        call    ClusterLBA          ;convert cluster to LBA
        xor cx,cx
        mov cl,BYTE[bpbSectorsPerCluster] 	;sectors to read
        call	ReadSectors
        push    bx
          
    ;compute next cluster
     
        mov	ax,WORD[cluster]  	;identify current cluster
        mov cx,ax               ;copy current cluster
        mov dx,ax               ;copy current cluster
        shr dx,0x0001           ;divide by two, Bitwise Logical Shifts
        add cx,dx               ;sum for (3/2)
        mov bx,0x0200           ;location of FAT in memory
        add bx,cx               ;index into FAT
        mov dx,WORD [bx]        ;read two bytes from FAT
        test	ax, 0x0001
        jnz	.ODD_CLUSTER
          
	;clusters have integrated parts, apply filtering
    .EVEN_CLUSTER:
     
        and     dx, 0000111111111111b               ; take low twelve bits
		jmp     .DONE
         
    .ODD_CLUSTER:
     
        shr     dx, 0x0004                          ; take high twelve bits
          
    .DONE:
     
        mov	WORD[cluster],dx	;store new cluster
        cmp dx, 0x0FF0	;test for end of file
        jb  LOAD_IMAGE
          
    DONE:
		int 0x16	;DEBUG PONIT, keyboard input
        mov     si, msgCRLF
        call    Print
        push    WORD 0x0050 ; you remember we loaded the cluster to this location
        push    WORD 0x0000
        retf	;RETF executes a far return: after popping IP/EIP, it then pops CS, and then increments the stack pointer by the optional argument if present.
          
    FAILURE:
     
        mov     si, msgFailure
        call    Print
        mov     ah, 0x00
        int     0x16                                ; await keypress
        int     0x19                                ; warm boot computer   
		
	;temporary fields to store CHS conversion results
	absoluteSector db 0x00
	absoluteHead   db 0x00
	absoluteTrack  db 0x00
	   
	datasector  dw 0x0000
	cluster     dw 0x0000
	ImageName   db "KRNLDR  SYS"	;target file name for stage 2 start
	msgStart  db 0x0D, 0x0A, "Stg1:Start KRNLDR.SYS", 0x0D, 0x0A, 0x00
	msgCRLF     db 0x0D, 0x0A, 0x00
	msgProgress db "-", 0x00
	msgFailure  db 0x0D, 0x0A, "ERROR:Press Key-Reboot", 0x0A, 0x00
	msgCantFindFile  db 0x0D, 0x0A, "NO KRNLDR.SYS!", 0x0A, 0x00
	
        TIMES 510-($-$$) DB 0
        DW 0xAA55
