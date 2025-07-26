[BITS 64]
section .text
global enable_ring0

enable_ring0:
    cli
    mov rax, cr0
    and rax, 0xFFFFFFFFFFFFFFFE
    mov cr0, rax
    lgdt [gdt_ptr]
    mov rax, cr0
    or rax, 1
    mov cr0, rax
    jmp 0x08:.ring0
.ring0:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    sti
    ret

section .data
gdt:
    dq 0
    dq 0x0020980000000000
    dq 0x0000920000000000
gdt_ptr:
    dw $ - gdt - 1
    dq gdt