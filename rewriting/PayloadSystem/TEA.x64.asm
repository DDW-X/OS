section .text
global tea_encrypt

tea_encrypt:
    ; rdi=data, rsi=key, rdx=rounds
    push r12
    push r13
    push r14
    push r15

    mov r12, rdi        ; data pointer
    mov r13, rsi        ; key pointer
    mov r14, rdx        ; rounds
    mov r15, 0x9E3779B9 ; delta

    mov eax, [r12]      ; v0
    mov ebx, [r12+4]    ; v1
    xor rcx, rcx        ; sum=0

.encrypt_loop:
    add ecx, r15d       ; sum += delta
    mov edx, ebx
    shl edx, 4
    add edx, [r13]      ; key[0]
    mov esi, ebx
    add esi, ecx
    mov edi, ebx
    shr edi, 5
    add edi, [r13+4]    ; key[1]
    xor edx, esi
    xor edx, edi
    add eax, edx        ; v0 += ((v1<<4)+key[0]) ^ (v1+sum) ^ ((v1>>5)+key[1])

    mov edx, eax
    shl edx, 4
    add edx, [r13+8]    ; key[2]
    mov esi, eax
    add esi, ecx
    mov edi, eax
    shr edi, 5
    add edi, [r13+12]   ; key[3]
    xor edx, esi
    xor edx, edi
    add ebx, edx        ; v1 += ((v0<<4)+key[2]) ^ (v0+sum) ^ ((v0>>5)+key[3])

    dec r14
    jnz .encrypt_loop

    mov [r12], eax
    mov [r12+4], ebx

    pop r15
    pop r14
    pop r13
    pop r12
    ret
    