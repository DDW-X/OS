[BITS 64]
section .text
global map_physical_memory

map_physical_memory:
    mov rax, cr3
    mov [orig_cr3], rax
    mov rax, [rdi]
    mov cr3, rax
    mov rax, cr0
    and rax, ~0x80000000
    mov cr0, rax
    mov rsi, 0xFFFFFF8000000000
    mov rcx, 512
.map_loop:
    mov rax, rdi
    or rax, 0x83
    mov [rsi], rax
    add rsi, 8
    add rdi, 0x200000
    loop .map_loop
    mov rax, cr0
    or rax, 0x80000000
    mov cr0, rax
    ret

section .bss
orig_cr3 resq 1