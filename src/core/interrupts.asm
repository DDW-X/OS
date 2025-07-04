[BITS 64]
section .text
global install_idt_handler

install_idt_handler:
    sidt [orig_idt]
    mov rax, [rdi]
    mov [idt_ptr + 2], rax
    mov word [idt_ptr], 0xFFF
    lidt [idt_ptr]
    ret

section .data
orig_idt:
    dw 0
    dq 0
idt_ptr:
    dw 0
    dq 0