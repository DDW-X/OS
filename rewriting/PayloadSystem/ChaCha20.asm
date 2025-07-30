section .text
global chacha20_encrypt

chacha20_encrypt:
    ; rdi=key, rsi=nonce, rdx=input, rcx=output, r8=length
    push r12
    mov r12, rdx

    ; Generate random key from entropy
    rdrand rax
    mov [rdi], rax
    rdrand rax
    mov [rdi+8], rax
    rdrand rax
    mov [rdi+16], rax
    rdrand rax
    mov [rdi+24], rax

    ; Init state
    movdqa xmm0, [init_state]
    movdqu xmm1, [rdi]
    movdqu xmm2, [rsi]

.encrypt_block:
    ; ChaCha20 quarter round operations
    ; Full implementation omitted for brevity
    ; ...
    ret

section .data
init_state: db 'expa', 'nd 32', '-byt', 'e k'

section .text
global chacha20_encrypt

chacha20_encrypt:
    ; rdi=key, rsi=nonce, rdx=input, rcx=output, r8=length
    ; Key expansion with hardware acceleration
    rdrand rax
    mov [rdi+32], rax  ; Extra entropy
    
    ; 20-round ChaCha with AVX2 acceleration
    vbroadcasti128 ymm0, [rdi]
    vbroadcasti128 ymm1, [rdi+16]
    vbroadcasti128 ymm2, [rsi]
    
.encrypt_block:
    ; ChaCha double round (x4)
    vpaddd ymm3, ymm0, ymm1
    vpxor ymm4, ymm3, ymm2
    vprold ymm4, 16, 0
    
    vpaddd ymm5, ymm0, ymm4
    vpxor ymm6, ymm5, ymm1
    vprold ymm6, 12, 0
    
    ; ... (20 rounds total)
    
    ; XOR with plaintext
    vmovdqu ymm7, [rdx]
    vpxor ymm8, ymm7, ymm0
    vmovdqu [rcx], ymm8
    
    add rdx, 32
    add rcx, 32
    sub r8, 32
    jg .encrypt_block
    
    ; Clear registers
    vzeroall
    ret

chacha20_encrypt:
    ; rdi=input, rsi=output, rdx=length, rcx=key, r8=nonce
    push r12
    push r13
    push r14
    
    mov r12, rdi        ; input
    mov r13, rsi        ; output
    mov r14, rdx        ; length
    mov r9, rcx         ; key
    mov r10, r8         ; nonce
    
    ; Initialize state
    vmovdqa xmm0, [init_constant]
    vmovdqu xmm1, [r9]       ; key[0:3]
    vmovdqu xmm2, [r9+16]    ; key[4:7]
    vmovdqu xmm3, [r10]      ; nonce and counter
    
.loop:
    ; ChaCha20 round implementation
    vpaddd xmm0, xmm0, xmm1
    vpxor xmm3, xmm3, xmm0
    vprold xmm3, 16, 0
    
    vpaddd xmm2, xmm2, xmm3
    vpxor xmm1, xmm1, xmm2
    vprold xmm1, 12, 0
    
    ; ... (20 rounds total)
    
    ; XOR with plaintext
    vmovdqu xmm4, [r12]
    vpxor xmm4, xmm4, xmm0
    vmovdqu [r13], xmm4
    
    add r12, 16
    add r13, 16
    sub r14, 16
    jg .loop
    
    vzeroall
    pop r14
    pop r13
    pop r12
    ret

section .rodata
init_constant: db 'expand 32-byte k'
