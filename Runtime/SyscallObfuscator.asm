; SyscallObfuscator.asm - Assembly Module

section .text
global mutate_syscall

mutate_syscall:
    ; Input: rax = syscall number
    ;        rdi, rsi, rdx, r10, r8, r9 = args
    ; Output: syscall result in rax
    
    push r11
    push rcx
    push rdx
    
    ; Randomize syscall number
    rdtsc
    and eax, 0x7F      ; Mask to valid syscall range
    mov r11, rax        ; Store randomized number
    
    ; Register permutation
    mov rcx, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, r10
    mov r10, r8
    mov r8, r9
    mov r9, rcx
    
    ; Randomize syscall method
    rdtsc
    test eax, 1
    jz .normal_syscall
    
    ; Alternative syscall methods
    test eax, 2
    jz .sysenter_method
    jmp .int80_method

.normal_syscall:
    mov rax, r11
    syscall
    jmp .done

.sysenter_method:
    mov rax, r11
    push rcx
    push rdx
    push r11
    lea rdx, [rel .sysenter_ret]
    mov rcx, rsp
    sysenter
.sysenter_ret:
    pop r11
    pop rdx
    pop rcx
    jmp .done

.int80_method:
    push r10
    push r8
    push r9
    mov eax, r11d
    mov ebx, edi
    mov ecx, esi
    mov edx, edx
    mov esi, r10d
    mov edi, r8d
    mov ebp, r9d
    int 0x80
    pop r9
    pop r8
    pop r10

.done:
    ; Restore register mapping
    mov rcx, r9
    mov r9, r8
    mov r8, r10
    mov r10, rdx
    mov rdx, rsi
    mov rsi, rdi
    mov rdi, rcx
    
    pop rdx
    pop rcx
    pop r11
    ret
    
global mutate_syscall

mutate_syscall:
    ; Input: rax = syscall number
    rdrand rbx
    mov rcx, rax

    ; Randomize syscall instruction
    test rbx, 1
    jz .normal_syscall
    mov r10, rcx
    db 0x0F, 0x05  ; Explicit syscall
    ret

.normal_syscall:
    ; Shuffle registers
    xchg rdi, rsi
    xchg rdx, r10
    syscall
    ; Restore registers
    xchg rdi, rsi
    xchg rdx, r10
    ret

global mutate_syscall

mutate_syscall:
    ; Input: rax = syscall number
    rdrand rbx
    mov rcx, rax

    ; Syscall gate randomization
    test rbx, 0x1
    jz .normal
    mov r10, rcx
    mov eax, 0
    db 0x0F, 0x05  ; syscall
    ret

.normal:
    ; Register permutation matrix
    mov r11, rax
    mov rax, rdi
    mov rdi, rsi
    mov rsi, rdx
    mov rdx, r10
    mov r10, r8
    mov r8, r9
    mov r9, r11

    ; Random syscall instruction
    test bl, 0b10
    jz .sysenter
    syscall
    jmp .restore
.sysenter:
    db 0x0F, 0x34  ; sysenter

.restore:
    ; Restore register mapping
    mov r9, r10
    mov r10, rdx
    mov rdx, rsi
    mov rsi, rdi
    mov rdi, rax
    mov rax, r11
    ret


syscall_wrapper:
    ; Input: rax=syscall number, rdi, rsi, rdx, r10, r8, r9
    ; Randomize syscall method
    rdrand rbx
    test bl, 1
    jz .sysenter
    syscall
    ret

.sysenter:
    ; Shuffle registers
    xchg rdi, rsi
    xchg rdx, r10
    mov r10, rcx
    db 0x0F, 0x34  ; sysenter
    ; Restore registers
    xchg rdi, rsi
    xchg rdx, r10
    ret
    