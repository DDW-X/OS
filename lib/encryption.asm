section .text

; رمزنگاری AES-256 با دستورات AES-NI
aes256_encrypt:
    ; rdi = آدرس ورودی
    ; rsi = آدرس خروجی
    ; rdx = آدرس کلید
    
    ; بارگذاری کلید
    movdqu xmm0, [rdx]
    movdqu xmm1, [rdx + 16]
    movdqu xmm2, [rdx + 32]
    movdqu xmm3, [rdx + 48]
    movdqu xmm4, [rdx + 64]
    movdqu xmm5, [rdx + 80]
    movdqu xmm6, [rdx + 96]
    movdqu xmm7, [rdx + 112]
    movdqu xmm8, [rdx + 128]
    movdqu xmm9, [rdx + 144]
    movdqu xmm10, [rdx + 160]
    movdqu xmm11, [rdx + 176]
    movdqu xmm12, [rdx + 192]
    movdqu xmm13, [rdx + 208]
    movdqu xmm14, [rdx + 224]
    
    ; بارگذاری داده
    movdqu xmm15, [rdi]
    
    ; 14 دور رمزنگاری
    aesenc xmm15, xmm0
    aesenc xmm15, xmm1
    aesenc xmm15, xmm2
    aesenc xmm15, xmm3
    aesenc xmm15, xmm4
    aesenc xmm15, xmm5
    aesenc xmm15, xmm6
    aesenc xmm15, xmm7
    aesenc xmm15, xmm8
    aesenc xmm15, xmm9
    aesenc xmm15, xmm10
    aesenc xmm15, xmm11
    aesenc xmm15, xmm12
    aesenclast xmm15, xmm13
    
    ; ذخیره خروجی
    movdqu [rsi], xmm15
    ret

; تابع درهم‌سازی SHA-256
sha256_hash:
    ; rdi = آدرس ورودی
    ; rsi = طول
    ; rdx = آدرس خروجی
    
    ; مقداردهی اولیه
    movdqu xmm0, [sha256_init + 0]
    movdqu xmm1, [sha256_init + 16]
    movdqu xmm2, [sha256_init + 32]
    movdqu xmm3, [sha256_init + 48]
    
    ; پردازش بلوک‌ها
.process_block:
    ; گسترش پیام
    movdqu xmm4, [rdi]
    pshufb xmm4, [sha256_bswap_mask]
    movdqu [rsp + 0], xmm4
    
    ; محاسبه درهم‌سازی
    sha256rnds2 xmm0, xmm1, xmm4
    ; ... (64 دور کامل)
    
    add rdi, 64
    sub rsi, 64
    jnz .process_block
    
    ; ذخیره نتیجه
    movdqu [rdx + 0], xmm0
    movdqu [rdx + 16], xmm1
    ret
    