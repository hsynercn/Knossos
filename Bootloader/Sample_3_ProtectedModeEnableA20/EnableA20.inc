%ifndef ENABLEA20_INC
%define ENABLEA20_INC

bits 16

;*******************************************
;
;8043 Keyboard Controller Status Register
;BIT 0:Output buffer status (0 = empty dont read 1 = full read)
;BIT 1:Input buffer status (0 = empty can write, 1 = full dont write)
;BIT 2:System Flag Meant to be cleared on reset and set by firmware (via. PS/2 Controller Configuration Byte) if the system passes self tests (POST)
;BIT 3:Command/data (0 = data written to input buffer is data for PS/2 device, 1 = data written to input buffer is data for PS/2 controller command)
;BIT 4:Unknown (chipset specific) May be "keyboard lock" (more likely unused on modern systems)
;BIT 5:Unknown (chipset specific) May be "receive time-out" or "second PS/2 port output buffer full"
;BIT 6:Time-out error (0 = no error, 1 = time-out error)
;BIT 7:Parity error (0 = no error, 1 = parity error)
;
;Enables the A20 for memory access, defines alternative methods
;
;*******************************************

EnableA20WithOut:
    push ax
    mov	al, 2
    out	0x92, al
    pop ax
    ret
	
EnableA20WithInterrupt:
	
    pusha
    mov ax, 0x2401
    int 0x15
    popa
    ret
	
DisableA20WithInterrupt:
    pusha
    mov ax, 0x2400
    int 0x15
    popa
    ret	
	
WaitBeforeRead:
    in      al,0x64			    ;read status register
    test    al,1			    ;test BIT0 (output buffer status), dont read yet if 0
    jz     WaitBeforeRead
    ret

WaitBeforeWrite:
    in al,0x64      ;read status
    test al,2       ;test BIT1 (input buffer status), dont write yet if 1
    jnz WaitBeforeWrite
    ret
    
%endif; ENABLEA20_INC
