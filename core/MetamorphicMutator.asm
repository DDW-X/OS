section .text
global mutate_code

mutate_code:
    ; rdi=code pointer, rsi=length
    push r12
    push r13
    mov r12, rdi
    mov r13, rsi
    
    ; Get entropy
    call harvest_entropy
    mov rcx, rax
    
.mutate_loop:
    ; Random instruction mutation
    mov al, [r12]
    xor al, cl          ; Simple XOR mutation
    rol cl, 3
    mov [r12], al
    
    ; Insert junk every 8 bytes
    test rcx, 7
    jnz .no_junk
    mov byte [r12+1], 0x90  ; NOP
    
.no_junk:
    inc r12
    dec r13
    jnz .mutate_loop
    
    pop r13
    pop r12
    ret
