section .text

; تاخیر دقیق
precise_delay:
    ; rdi = میکروثانیه
    rdtsc
    mov r8, rdx
    shl r8, 32
    or r8, rax
    mov r9, rdi
    imul r9, CPU_FREQ_MHZ
    
.wait_loop:
    pause
    rdtsc
    mov r10, rdx
    shl r10, 32
    or r10, rax
    sub r10, r8
    cmp r10, r9
    jb .wait_loop
    ret

; محاسبه CRC32
crc32:
    ; rdi = آدرس داده
    ; rsi = طول
    xor eax, eax
    mov rcx, rsi
.crc_loop:
    crc32 eax, byte [rdi]
    inc rdi
    loop .crc_loop
    ret

; تولید عدد تصادفی سخت‌افزاری
hardware_rng:
    rdrand rax
    jnc hardware_rng ; تکرار در صورت شکست
    ret

; رمزنگاری سریع با AES-NI
aes_encrypt:
    ; rdi = آدرس ورودی
    ; rsi = آدرس خروجی
    ; rdx = آدرس کلید
    movdqu xmm0, [rdx]
    movdqu xmm1, [rdi]
    aesenc xmm1, xmm0
    movdqu [rsi], xmm1
    ret
    