[BITS 64]
section .text
global enter_smm

enter_smm:
    mov dx, 0xB2
    mov al, 0x0F
    out dx, al
    rsm
    ret