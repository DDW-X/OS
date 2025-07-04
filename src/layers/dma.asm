[BITS 64]
section .text
global dma_transfer

dma_transfer:
    mov al, 0x04
    out 0x0A, al
    xor al, al
    out 0x0C, al
    mov al, 0x48
    out 0x0B, al
    mov ax, di
    out 0x04, al
    mov al, ah
    out 0x04, al
    mov ax, si
    out 0x05, al
    mov al, ah
    out 0x05, al
    mov ax, cx
    out 0x05, al
    mov al, ah
    out 0x05, al
    mov al, 0x87
    out 0x0A, al
    ret