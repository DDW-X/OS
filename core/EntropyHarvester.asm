section .text
global harvest_entropy

harvest_entropy:
    ; Combine multiple hardware entropy sources
    rdrand rax        ; Intel DRNG
    rdseed rbx        ; True random seed
    rdtsc             ; Time stamp counter
    shl rdx, 32
    or rax, rdx
    xor rax, rbx      ; Combine sources
    
    ; Add memory layout entropy
    mov rcx, [rsp]    ; Stack address
    xor rax, rcx
    
    ; Process ID entropy
    mov rdi, 0x14     ; SYS_getpid
    call mutate_syscall
    xor rax, rdi
    
    ret
    