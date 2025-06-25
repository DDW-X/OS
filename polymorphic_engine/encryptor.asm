section .text

; رمزنگاری کد با الگوریتم پویا
encrypt_code:
    ; انتخاب الگوریتم تصادفی
    rdrand eax
    and eax, 0x7  ; 8 الگوریتم مختلف
    jmp [encryption_table + rax*8]

encryption_table:
    dq encrypt_xor
    dq encrypt_aes
    dq encrypt_rc4
    dq encrypt_chacha
    dq encrypt_blowfish
    dq encrypt_serpent
    dq encrypt_twofish
    dq encrypt_custom

encrypt_xor:
    ; رمزنگاری XOR ساده
    mov rdi, [code_start]
    mov rcx, [code_size]
    mov eax, [xor_key]
.encrypt_loop:
    xor [rdi], eax
    rol eax, 3
    inc rdi
    loop .encrypt_loop
    ret

encrypt_aes:
    ; رمزنگاری AES با دستورات AES-NI
    movdqu xmm0, [aes_key]
    mov rdi, [code_start]
    mov rcx, [code_size]
    shr rcx, 4  ; تعداد بلوک‌های 16 بایتی
.aes_loop:
    movdqu xmm1, [rdi]
    aesenc xmm1, xmm0
    movdqu [rdi], xmm1
    add rdi, 16
    loop .aes_loop
    ret

encrypt_rc4:
    ; رمزنگاری RC4
    call initialize_rc4_state
    mov rdi, [code_start]
    mov rcx, [code_size]
.rc4_loop:
    call rc4_keystream_byte
    xor [rdi], al
    inc rdi
    loop .rc4_loop
    ret

; تغییر کلید رمزنگاری در هر اجرا
change_encryption_key:
    rdrand eax
    mov [xor_key], eax
    rdrand rax
    mov [aes_key], rax
    rdrand rax
    mov [aes_key+8], rax
    ret
    