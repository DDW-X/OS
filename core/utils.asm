%include "scorch_macros.inc"

; انتظار برای SPI
spi_wait:
    push rcx
    mov rcx, SPI_TIMEOUT
.wait_loop:
    dec rcx
    jz .timeout
    test byte [SPI_BASE_ADDR + SPI_STATUS_REG], SPI_STATUS_BUSY
    jnz .wait_loop
.timeout:
    pop rcx
    ret

; انتظار طولانی برای SPI
spi_wait_long:
    push rcx
    mov rcx, SPI_LONG_TIMEOUT
    jmp spi_wait.wait_loop

; انتظار برای EC
ec_wait:
    push rcx
    mov rcx, EC_TIMEOUT
.ec_wait_loop:
    mov dx, EC_INDEX_PORT
    mov al, EC_STATUS_REG
    out dx, al
    mov dx, EC_DATA_PORT
    in al, dx
    test al, EC_BUSY_FLAG
    jz .ec_done
    loop .ec_wait_loop
.ec_done:
    pop rcx
    ret

; تشخیص نوع فرم‌ور
detect_firmware_type:
    ; بررسی امضای UEFI
    mov rsi, 0xFFFFFFF0 ; آدرس ثابت System Table
    cmp dword [rsi], 'EFI'
    je .uefi
    
    ; تشخیص BIOS سنتی
    mov rax, FIRMWARE_BIOS
    ret
.uefi:
    mov rax, FIRMWARE_UEFI
    ret
    