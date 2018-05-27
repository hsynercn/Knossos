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
;
;Output Data Format
;Bit 0: System Reset 0: Reset computer 1: Normal operation
;Bit 1: A20 0: Disabled 1: Enabled
;Bit 2-3: Undefined
;Bit 4: Input buffer full
;Bit 5: Output Buffer Empty
;Bit 6: Keyboard Clock 0: High-Z 1: Pull Clock Low
;Bit 6: Keyboard Data 0: High-Z 1: Pull Data Low
;
;Keyboard Controller Commands:
;0xAD   Disable Keyboard
;0xAE   Enable Keyboard
;
;Enables the A20 for memory access, defines alternative methods
;
;*******************************************

;*******************************************
;   Uses sysyem control port, depends on the manufacturer
;*******************************************
EnableA20WithOut:
    push ax
    mov al, 2
    out 0x92, al
    pop ax
    ret

;*******************************************
;   Uses BIOS interrupt, depends on BIOS, not going to work after PMode switch
;*******************************************
EnableA20WithInterrupt:
    
    pusha
    mov ax, 0x2401
    int 0x15
    popa
    ret
    
;*******************************************
;   Uses BIOS interrupt, depends on BIOS, not going to work after PMode switch
;*******************************************    
DisableA20WithInterrupt:
    pusha
    mov ax, 0x2400
    int 0x15
    popa
    ret 

;*******************************************
;   Sends direct comamnd to keyboard controller, not supported by every keyboard controller
;*******************************************
EnableA20WithKeyboardController:
    cli
    push    ax
    mov al, 0xdd
    out 0x64, al
    pop ax
    ret

;*******************************************
;   Most reliable method, changes the keyboard output port
;*******************************************
EnableA20WithOutputPort:
    cli
    pusha
    
    call WaitBeforeWrite
    mov al,0xAD ;disable keyboard
    out 0x64, al
    
    call WaitBeforeWrite
    mov al,0xD0 ;we want to read output port
    out 0x64,al
    call WaitBeforeRead ;result is ready
    in al,0x60 ;store the result
    push ax

    call WaitBeforeWrite
    mov al,0xD1;we want to write output port, going to write A20 enable bit
    out 0x64,al 
    call WaitBeforeWrite
    
    pop ax
    or al,2
    out 0x64,al
    
    call WaitBeforeWrite
    mov al,0xAE ;enable keyboard
    out 0x64, al
    call WaitBeforeWrite
    
    popa
    sti
    ret
    
WaitBeforeRead:
    in al,0x64              ;read status register
    test al,1               ;test BIT0 (output buffer status), dont read yet if 0
    jz WaitBeforeRead
    ret

WaitBeforeWrite:
    in al,0x64      ;read status
    test al,2       ;test BIT1 (input buffer status), dont write yet if 1
    jnz WaitBeforeWrite
    ret
    
%endif; ENABLEA20_INC
