section .text

; مبهم‌سازی پویای کد
dynamic_obfuscation:
    ; تولید کلید تصادفی
    rdrand eax
    mov [xor_key], eax

    ; مبهم‌سازی کد با XOR
    mov rdi, [code_start]
    mov rcx, [code_size]
    mov eax, [xor_key]
.obfuscate_loop:
    xor [rdi], eax
    rol eax, 3
    inc rdi
    loop .obfuscate_loop

    ; افزودن کد بی‌معنی
    call insert_junk_code
    ret

; تغییر شکل کد در حین اجرا
runtime_morphing:
    ; رمزگشایی بخش بعدی کد
    mov rdi, [next_code_block]
    mov rsi, [block_size]
    call decrypt_code

    ; تغییر الگوریتم مبهم‌سازی
    call change_obfuscation_algorithm

    ; مبهم‌سازی مجدد کد اجراشده
    mov rdi, [current_code_block]
    mov rsi, [block_size]
    call encrypt_code
    ret

; تزریق کد بی‌معنی
insert_junk_code:
    ; تولید کد تصادفی
    rdrand eax
    mov [junk_code], eax

    ; درج دستورات بی‌تاثیر
    mov ecx, 100
.junk_loop:
    nop
    xchg eax, ebx
    clc
    stc
    loop .junk_loop
    ret
    