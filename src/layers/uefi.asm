[BITS 64]
section .text
global call_uefi_service

call_uefi_service:
    mov rax, [uefi_services]
    jmp qword [rax + rdi * 8]