section .text
global self_modify_entry

self_modify_entry:
    lea r15, [rel modification_point]
    mov byte [r15], 0x90  ; NOP placeholder

    rdrand rax
    test al, 0b111        ; Random modification
    jz .mod1
    mov byte [r15], 0xEB  ; JMP short
    mov byte [r15+1], 0xFE
    jmp .end
.mod1:
    mov byte [r15], 0x75  ; JNE
    mov byte [r15+1], 0xFC

.end:
    call modification_point
    ret

modification_point:
    nop
    nop
    ; This area gets modified at runtime
    ret
    